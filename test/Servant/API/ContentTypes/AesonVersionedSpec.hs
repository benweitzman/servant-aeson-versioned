
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

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
         "foo" :> Get '[JSONVersioned V1, JSONVersioned V2] Person
    :<|> "foos" :> Get '[JSONVersioned V1, JSONVersioned V2] [Person]

jsonVersionApi :: Proxy JSONVersionApi
jsonVersionApi = Proxy

jsonVersionServer :: Server JSONVersionApi
jsonVersionServer = return alice :<|> return [alice]

spec :: Spec
spec = do
  describe "serializing" $ do
    with (return $ serve jsonVersionApi jsonVersionServer) $ do
      it "should serialize v1" $ do
        response <- request methodGet "/foo" [("Accept", "application/json;version=1.0")] ""
        liftIO $ decode' (simpleBody response) `shouldBe` Just (toJSON (Tagged alice :: Tagged V1 Person))
      it "should serialize v2" $ do
        response <- request methodGet "/foo" [("Accept", "application/json;version=2.0")] ""
        liftIO $ decode' (simpleBody response) `shouldBe` Just (toJSON (Tagged alice :: Tagged V2 Person))




---------------
-- Datatypes --
---------------

data Person = Person String Int deriving (Generic, Eq)

alice :: Person
alice = Person "alice" 25

instance ToJSON Person

instance ToJSON (Tagged V1 Person) where
    toJSON (Tagged (Person name _)) = object [ "name" .= name ]

instance ToJSON (Tagged V2 Person) where
    toJSON (Tagged (Person name age)) = object [ "name" .= name
                                               , "age" .= age]
