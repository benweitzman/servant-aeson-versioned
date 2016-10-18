{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Servant.API.ContentTypes.AesonVersioned where

import Servant.API.ContentTypes

import Control.Applicative

import Control.Error

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS

import Data.CaseInsensitive

import Data.Aeson
import Data.Aeson.Versions

import Data.Functor.Identity
import Data.Functor.Compose

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


getContentType :: BS.ByteString -> MediaType -> Maybe MediaType
getContentType contentTypeHeader defaultMedia = case parseAccept contentTypeHeader of
  Nothing -> Nothing
  Just media -> if media `matches` ("application" // "json")
                then Just $ if media /? "version"
                            then media
                            else defaultMedia
                else Nothing


instance {-# OVERLAPPING #-} AllMime list => AllMime (JSONVersioned ': list) where
  allMime _ = allMime (Proxy :: Proxy list)


instance AllCTRender '[] a where
  handleAcceptH _ _ _ = Nothing


instance {-# OVERLAPPING #-} (AllCTRender list String) => AllCTRender (JSONVersioned ': list) String where
  handleAcceptH p ah@(AcceptHeader acceptHeader) a =
    handleAcceptH (Proxy :: Proxy '[JSONVersioned]) ah (Identity a)
    <|>
    handleAcceptH (Proxy :: Proxy list) ah a

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

instance {-# OVERLAPPING #-} (AllCTUnrender list String) => AllCTUnrender (JSONVersioned ': list) String where
  handleCTypeH _ ch reqBody =
    (getCompose . fmap runIdentity . Compose $ handleCTypeH (Proxy :: Proxy '[JSONVersioned]) ch reqBody)
    <|>
    handleCTypeH (Proxy :: Proxy list) ch reqBody


instance (DeserializedVersion a
         ,AllCTUnrender list a) => AllCTUnrender (JSONVersioned ': list) a where
  handleCTypeH _ ch reqBody =
    (getCompose . fmap runIdentity . Compose $ handleCTypeH (Proxy :: Proxy '[JSONVersioned]) ch reqBody)
    <|>
    handleCTypeH (Proxy :: Proxy list) ch reqBody

instance {-# OVERLAPPING #-} (TraversableFromJSON t, DeserializedVersion a
         ,AllCTUnrender list (t a)) => AllCTUnrender (JSONVersioned ': list) (t a) where
  handleCTypeH _ contentTypeHeader reqBody = (do
    let deserializers' = deserializers
        defaultVersion = fst $ M.findMax deserializers'
    media <- getContentType (LBS.toStrict contentTypeHeader) $
      "application" // "json" /: ("version", BS.pack $ show defaultVersion)
    let mVersionBS = original <$> media /. "version"
    version <- maybe (pure defaultVersion) parseVersion mVersionBS
    deserializer <- M.lookup version deserializers'
    return $ do
      value <- eitherDecodeLenient reqBody
      note "Valid JSON but didn't match version" $ deserialize deserializer value)
    <|>
    handleCTypeH (Proxy :: Proxy list) contentTypeHeader reqBody
