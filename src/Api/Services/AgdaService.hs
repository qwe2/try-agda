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
import qualified Data.Text as T
import System.Random
import qualified Data.Text.IO as D
import Data.UUID
import Agda.TypeChecking.Monad.Base    (TCErr(PatternErr, IOException, Exception, TypeError), envRange, clEnv, clValue)
import Control.Monad.IO.Class

import Type.SnippetError
import Type.Snippet

data AgdaService = AgdaService

makeLenses ''AgdaService

agdaRoutes :: [(B.ByteString, Handler b AgdaService ())]
agdaRoutes = [("/typecheck", method POST typeCheck)]

typeCheck :: Handler b AgdaService ()
typeCheck = do
  text <- getRequestBody
  let snippet = (decode text) :: Maybe Snippet
  case snippet of
    Nothing -> do
      modifyResponse $ setResponseCode 400
    Just sn -> do
      file <- liftIO $ putInFile (code sn)
      err <- liftIO $ doCheck file
      writeLBS $ encode (fmap match err)
    where
      mkSe err rng = SnippetError { errorMessage = err
                                  , ranges = fmap (\(s, p) -> (pack `fmap` s, p)) (rngToPos rng) 
                                  }

      match (Exception rng str) = mkSe str rng
      match PatternErr = SnippetError "Pattern error" []
      match (IOException rng ex) = mkSe (show ex) rng
      match (TypeError _ cls) =
        let rng = envRange $ clEnv cls in
        let msg = show $ clValue cls in
        mkSe msg rng

moduleName :: String -> Text
moduleName fn = T.concat [ "module ", pack fn, " where\n" ]

putInFile :: Text -> IO String
putInFile content = do
  {-
   -uuid <- randomIO
   -let fn = toString uuid
   -let file = "tmp/" ++ fn ++ ".agda"
   -}
  let file = "tmp/a.agda"
  D.writeFile file $ moduleName "a"
  D.appendFile file content
  return file

agdaServiceInit :: SnapletInit b AgdaService
agdaServiceInit = makeSnaplet "agda" "Agda Service" Nothing $ do
  addRoutes agdaRoutes
  return AgdaService
