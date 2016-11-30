{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.API.ContentTypes.AesonVersioned where

import Servant.API

import qualified Data.ByteString.Char8 as BS

import Data.Aeson
import Data.Aeson.Versions

import Data.Proxy
import Data.Tagged

import Data.Functor.Identity

import Control.Error

import GHC.TypeLits

import Network.HTTP.Media hiding (Accept)

type family Map (f :: k -> k') (xs :: [k]) :: [k'] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

type family Append xs ys where
    Append '[] ys = ys
    Append (x ': xs) ys = x ': Append xs ys


type family Versioned verb rest a where
  Versioned ReqBody rest (f a) = ReqBody (Append (Map JSONVersioned (DeserializerVersions a)) rest) (f a)
  Versioned ReqBody rest a = ReqBody (Append (Map JSONVersioned (DeserializerVersions a)) rest) a
  Versioned verb rest (f a) = verb (Append (Map JSONVersioned (SerializerVersions a)) rest) (f a)
  Versioned verb rest a = verb (Append (Map JSONVersioned (SerializerVersions a)) rest) a

type family ReqBodyV rest a where
    ReqBodyV rest (f a) = ReqBody (Append (Map JSONVersioned (DeserializerVersions a)) rest) (f a)
    ReqBodyV rest a = ReqBody (Append (Map JSONVersioned (DeserializerVersions a)) rest) a

data JSONVersioned (v :: Version Nat Nat)

instance KnownVersion v => Accept (JSONVersioned v) where
    contentType _ =  "application" // "json" /: ("version", BS.pack . show $ versionVal (Proxy :: Proxy v))

newtype UsingSingle a = UsingSingle a

instance {-# OVERLAPPING #-}
  (FailableToJSON (Tagged v a), KnownVersion v) => MimeRender (JSONVersioned v) (UsingSingle a) where
  mimeRenderMaybe _ (UsingSingle val) = encode <$> mToJSON (Tagged val :: Tagged v a)

instance (FailableToJSON (Tagged v a), KnownVersion v) => MimeRender (JSONVersioned v) a where
    mimeRenderMaybe _ val = encode <$> mToJSON (Tagged val :: Tagged v a)

instance {-# OVERLAPPING #-}
    (FailableToJSON (Tagged v a), KnownVersion v, CatMaybes f, FunctorToJSON f)
    => MimeRender (JSONVersioned v) (f a) where
    mimeRenderMaybe _ val = encode <$> runSerializer serializer val

        where  serializer :: Serializer a
               serializer = snd $ getSerializer (Proxy :: Proxy v)


instance {-# OVERLAPPABLE #-} (FromJSON (Tagged v a), KnownVersion v) => MimeUnrender (JSONVersioned v) a where
  mimeUnrender _ body = do
    val <- eitherDecode body
    a <- note "Invalid json version" $ runDeserializer deserializer val
    return $ runIdentity a

    where  deserializer :: Deserializer a
           deserializer = snd $ getDeserializer (Proxy :: Proxy v)

instance {-# OVERLAPPING #-} (TraversableFromJSON t, FromJSON (Tagged v a), KnownVersion v) => MimeUnrender (JSONVersioned v) (t a) where
  mimeUnrender _ body = do
    val <- eitherDecode body
    note "Invalid json version" $ runDeserializer deserializer val

    where  deserializer :: Deserializer a
           deserializer = snd $ getDeserializer (Proxy :: Proxy v)
