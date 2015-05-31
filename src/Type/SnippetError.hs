{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Type.SnippetError where

import qualified Agda.Syntax.Position as P
import Agda.Utils.FileName   ( filePath)
import Data.Typeable
import Data.Aeson
import Data.Text             ( Text, pack)
import Data.JSON.Schema
import Data.Int              ( Int32)
import GHC.Generics
import System.FilePath.Posix

data Position = Position
  { line :: Int32
  , col :: Int32
  } deriving (Show, Generic, Typeable)

instance JSONSchema Position where schema = gSchema
instance ToJSON     Position
instance FromJSON   Position

data Range = Range
  {  start :: (Maybe Text, Position)
  ,  end :: (Maybe Text, Position)
  } deriving (Show, Generic, Typeable)

instance JSONSchema Range where schema = gSchema
instance ToJSON     Range
instance FromJSON   Range

data SnippetError =
    Ok
  | SnippetError
      { errorMessage :: String
      , ranges       :: [Range]
      } deriving (Show, Generic, Typeable)

instance JSONSchema SnippetError where schema = gSchema
instance ToJSON     SnippetError
instance FromJSON   SnippetError

rngToPos :: P.Range -> [Range]
rngToPos (P.Range rngs) =  map f rngs
  where
    pth = fmap (takeFileName . filePath)

    f :: P.Interval -> Range
    f x =
      let (s, e) = (P.iStart x, P.iEnd x) in
      let filest = fmap pack (pth $ P.srcFile s) in
      let filee  = fmap pack (pth $ P.srcFile e) in
      Range
        { start = (filest, Position { line = P.posLine s, col = P.posCol s })
        , end   = (filee, Position { line = P.posLine e, col = P.posCol e })
        }

