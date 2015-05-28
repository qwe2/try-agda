{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.AgdaService where

import Control.Lens
import Data.Aeson
import Snap.Core
import Snap.Snaplet
import AgdaComm.TypeCheck
import qualified Data.ByteString.Char8 as B
import Data.Text
import System.Random
import qualified Data.Text.IO as D
import Data.UUID
import Control.Monad.IO.Class
import System.Directory ( removeFile, createDirectoryIfMissing )

import Type.Snippet

data AgdaService = AgdaService

makeLenses ''AgdaService

agdaRoutes :: [(B.ByteString, Handler b AgdaService ())]
agdaRoutes = [("/typecheck", method POST typeCheck)]

typeCheck :: Handler b AgdaService ()
typeCheck = do
  text <- readRequestBody (2 * 1024 * 1024) -- 2 megabytes
  let snippet = decode text :: Maybe Snippet
  case snippet of
    Nothing ->
      modifyResponse $ setResponseCode 400
    Just sn -> do
      file <- liftIO $ writeAgdaFile (code sn)
      err <- liftIO $ doCheck file
      liftIO $ removeFile file
      writeLBS $ encode err

writeAgdaFile :: Text -> IO String
writeAgdaFile content = do
  uuid <- randomIO
  let fn = toString uuid
  let file = "tmp/" ++ fn ++ ".agda"
  D.writeFile file content
  return file

agdaServiceInit :: SnapletInit b AgdaService
agdaServiceInit = makeSnaplet "agda" "Agda Service" Nothing $ do
  liftIO $ createDirectoryIfMissing False "tmp"
  addRoutes agdaRoutes
  return AgdaService
