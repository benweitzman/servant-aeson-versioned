{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyCase #-}

module Servant.API.ContentTypes.AesonVersioned where

import Servant.API.ContentTypes

import Control.Applicative

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS

import Data.CaseInsensitive

import Data.Aeson
import Data.Aeson.Versions

import Data.Functor.Identity

import Data.Maybe
import Data.Monoid

import Data.Proxy

import Text.Read

import qualified Data.Map as M

import Network.HTTP.Media


data JSONVersioned


noParams :: MediaType -> MediaType
noParams media = original (mainType media) // original (subType media)

refineAccept :: MediaType -- ^ default
             -> MediaType -- ^ input
             -> Maybe MediaType
refineAccept defaultMedia media
  | ("application" // "json") `matches` noParams media = Just $ case media /. "version" of
    Just v -> ("application" // "json") /: ("version", original v)
    Nothing -> defaultMedia
  | otherwise = Nothing

getAcceptMedia :: BS.ByteString -> MediaType -> Maybe MediaType
getAcceptMedia acceptHeader defaultMedia = case sequence $ parseAccept <$> BS.split ',' acceptHeader of
  Nothing -> Nothing
  Just medias -> case getFirst . mconcat $ First . refineAccept defaultMedia <$> medias of
    Just media -> Just media
    Nothing -> Nothing


instance {-# OVERLAPPING #-} AllMime list => AllMime (JSONVersioned ': list) where
  allMime _ = allMime (Proxy :: Proxy list)


instance AllCTRender '[] a where
  handleAcceptH _ _ _ = Nothing


instance {-# OVERLAPPABLE #-} (SerializedVersion a, AllCTRender list a) => AllCTRender (JSONVersioned ': list) a where
  handleAcceptH p ah@(AcceptHeader acceptHeader) a =
    handleAcceptH (Proxy :: Proxy '[JSONVersioned]) ah (Identity a)
    <|>
    handleAcceptH (Proxy :: Proxy list) ah a

instance (SerializedVersion a, FunctorToJSON f
         ,CatMaybes f, AllCTRender list (f a)) => AllCTRender (JSONVersioned ': list) (f a) where
  handleAcceptH _ ah@(AcceptHeader acceptHeader) a = (do
    let serializers' = serializers
        defaultVersion = fst $ M.findMax serializers'
    media <- getAcceptMedia acceptHeader $ "application" // "json" /: ("version", BS.pack $ show defaultVersion)
    let mVersionBS = original <$> media /. "version"
    version <- maybe (pure defaultVersion) parseVersion mVersionBS
    serializer <- M.lookup version serializers'
    value <- serialize serializer a
    return $ (encode value, LBS.fromStrict $ renderHeader media))
    <|>
    handleAcceptH (Proxy :: Proxy list) ah a

parseVersion :: BS.ByteString -> Maybe (Version Integer Integer)
parseVersion v = readMaybe (BS.unpack v)
