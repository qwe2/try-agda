{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

module Type.Snippet where

import Data.Aeson
import Data.Typeable
import Data.JSON.Schema
import Data.Text (Text)
import GHC.Generics

data Snippet = Snippet { code :: Text } deriving (Show, Generic, Typeable)

instance JSONSchema Snippet where schema = gSchema
instance ToJSON     Snippet
instance FromJSON   Snippet
