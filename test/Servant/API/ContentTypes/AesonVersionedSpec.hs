
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.API.ContentTypes.AesonVersionedSpec where

import Test.Hspec

import Data.Aeson
import Data.Aeson.Versions

import Data.Proxy
import Data.Tagged

import GHC.Generics (Generic)

import Network.Wai.Test hiding (request)

import Network.HTTP.Types.Method

import Servant.API
import Servant.Server

import Test.Hspec.Wai

import Servant.API.ContentTypes.AesonVersioned

type JSONVersionApi =
         "foo" :> Versioned Get '[] Person
    :<|> "foos" :> Versioned Get '[] [Person]
    :<|> "bar" :> Versioned ReqBody '[] Person :> Versioned Get '[] Person
    :<|> "bars" :> Versioned ReqBody '[] [Person] :> Versioned Get '[] [Person]

jsonVersionApi :: Proxy JSONVersionApi
jsonVersionApi = Proxy

jsonVersionServer :: Server JSONVersionApi
jsonVersionServer =
       return alice
  :<|> return [alice]
  :<|> return
  :<|> return

spec :: Spec
spec = with (return $ serve jsonVersionApi jsonVersionServer) $ do
  describe "serializing" $ do
    it "should serialize v1" $ do
      response <- request methodGet "/foo" [("Accept", "application/json;version=1.0")] ""
      liftIO $ decode' (simpleBody response) `shouldBe` Just (toJSON (Tagged alice :: Tagged V1 Person))
    it "should serialize v1" $ do
      response <- request methodGet "/foo" [("Accept", "application/json;version=1.0")] ""
      liftIO $ decode' (simpleBody response) `shouldBe` Just (toJSON (Tagged alice :: Tagged V1 Person))
    it "should serialize v2" $ do
      response <- request methodGet "/foo" [("Accept", "application/json;version=2.0")] ""
      liftIO $ decode' (simpleBody response) `shouldBe` Just (toJSON (Tagged alice :: Tagged V2 Person))
    it "should fail to serialize v3" $ do
      request methodGet "/foo" [("Accept", "application/json;version=3.0")] "" `shouldRespondWith` 406
    it "should fail to serialize non existent version" $ do
      request methodGet "/foo" [("Accept", "application/json;version=4.0")] "" `shouldRespondWith` 406
    it "should serialize [v1]" $ do
      response <- request methodGet "/foos" [("Accept", "application/json;version=1.0")] ""
      liftIO $ decode' (simpleBody response) `shouldBe` Just (toJSON [toJSON (Tagged alice :: Tagged V1 Person)])
    it "should serialize [v2]" $ do
      response <- request methodGet "/foos" [("Accept", "application/json;version=2.0")] ""
      liftIO $ decode' (simpleBody response) `shouldBe` Just (toJSON [toJSON (Tagged alice :: Tagged V2 Person)])
    it "should serialize empty [v3]" $ do
      response <- request methodGet "/foos" [("Accept", "application/json;version=3.0")] ""
      liftIO $ decode' (simpleBody response) `shouldBe` Just (toJSON ([] :: [Int]))
  describe "deserializing" $ do
    it "should deserialize v1" $ do
      let body = encode (Tagged alice :: Tagged V2 Person)
      request methodGet "/bar" [("Content-Type", "application/json;version=1.0")] body `shouldRespondWith` 200
    it "should deserialize v2" $ do
      let body = encode (Tagged alice :: Tagged V1 Person)
      request methodGet "/bar" [("Content-Type", "application/json;version=2.0")] body `shouldRespondWith` 200
    it "should fail to deserialize non existent version" $ do
      let body = encode (Tagged alice :: Tagged V1 Person)
      request methodGet "/bar" [("Content-Type", "application/json;version=3.0")] body `shouldRespondWith` 415
    it "should fail to deserialize bad data" $ do
      let body = encode (Tagged alice :: Tagged V1 Person)
      request methodGet "/bar" [("Content-Type", "application/json;version=1.0")] body `shouldRespondWith` 400
    it "should deserialize [v1]" $ do
      let body = encode $ toJSON [toJSON (Tagged alice :: Tagged V2 Person)]
      request methodGet "/bars" [("Content-Type", "application/json;version=1.0")] body `shouldRespondWith` 200
    it "should deserialize [v2]" $ do
      let body = encode $ toJSON [toJSON (Tagged alice :: Tagged V1 Person)]
      request methodGet "/bars" [("Content-Type", "application/json;version=2.0")] body `shouldRespondWith` 200







---------------
-- Datatypes --
---------------

data Person = Person String Int deriving (Generic, Eq)

alice :: Person
alice = Person "alice" 25

-- instance ToJSON Person

instance ToJSON (Tagged V1 Person) where
    toJSON (Tagged (Person name _)) = object [ "name" .= name ]

instance ToJSON (Tagged V2 Person) where
    toJSON (Tagged (Person name age)) = object [ "name" .= name
                                               , "age" .= age
                                               ]

instance FailableToJSON (Tagged V3 Person) where
  mToJSON (Tagged (Person name age))
    | age >= 25 = Nothing
    | otherwise = Just $ object [ "name" .= name
                                , "age" .= age
                                ]

instance SerializedVersion Person where
  type SerializerVersions Person = '[V1, V2, V3]

instance FromJSON (Tagged V1 Person) where
  parseJSON (Object o) = Tagged <$> (Person <$> o .: "name"
                                            <*> o .: "age")
  parseJSON _ = mempty

instance FromJSON (Tagged V2 Person) where
  parseJSON (Object o) = Tagged <$> (Person <$> o .: "name"
                                            <*> pure 25)
  parseJSON _ = mempty

instance DeserializedVersion Person where
  type DeserializerVersions Person = '[V1, V2]
