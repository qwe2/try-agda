{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Type.SnippetError where

import qualified Agda.Syntax.Position as P
import Agda.Utils.FileName (filePath)
import Data.Typeable
import Data.Aeson
import Data.Text (Text)
import Data.JSON.Schema
import Data.Int (Int32)
import GHC.Generics
import System.FilePath.Posix

data Position = Position
  { line :: Int32
  , col :: Int32
  } deriving (Show, Generic, Typeable)

instance JSONSchema Position where schema = gSchema
instance ToJSON     Position
instance FromJSON   Position

data SnippetError =
    Ok
  | SnippetError
      { errorMessage :: String
      , ranges       :: [(Maybe Text, Position)]
      } deriving (Show, Generic, Typeable)

instance JSONSchema SnippetError where schema = gSchema
instance ToJSON     SnippetError
instance FromJSON   SnippetError

rngToPos :: P.Range -> [(Maybe FilePath, Position)]
rngToPos (P.Range rngs) =  rngs >>= f
  where
    pth = fmap (takeFileName . filePath)

    f :: P.Interval -> [(Maybe FilePath, Position)]
    f x =
      let (s, e) = (P.iStart x, P.iEnd x) in
      let filest = pth $ P.srcFile s in
      let filee  = pth $ P.srcFile e in
      [ (filest, Position { line = P.posLine s, col = P.posCol s })
      , (filee, Position { line = P.posLine e, col = P.posCol e })
      ]

