{-# LANGUAGE ScopedTypeVariables #-}

module AgdaComm.TypeCheck (doCheck) where

import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Agda.Interaction.Imports        ( typeCheckMain, MaybeWarnings )
import Agda.Interaction.Options        ( defaultOptions, optIncludeDirs )
import Agda.TypeChecking.Monad.Base    ( Interface, runTCMTop, TCErr(PatternErr, IOException, Exception, TypeError), envRange, clEnv, clValue )
import Agda.TypeChecking.Monad.Options ( setCommandLineOptions )
import Agda.Utils.FileName             ( absolute )
import System.FilePath.Posix
import Data.Text
import Text.PrettyPrint.HughesPJ       ( render )

import Type.SnippetError

doCheck :: String -> IO (Maybe SnippetError)
doCheck relative = do
  let dir = takeDirectory relative
  absol <- absolute relative

  r :: Either TCErr (Interface, MaybeWarnings) <- liftIO $ runTCMTop $
    do setCommandLineOptions (defaultOptions { optIncludeDirs = Left [".", dir] })
       typeCheckMain absol

  case r of
    Left tcerr ->
      return $ Just (match tcerr)
    Right _ ->
      return Nothing
  where
    mkSe err rng = SnippetError { errorMessage = err
                                , ranges = fmap (\(s, p) -> (pack `fmap` s, p)) (rngToPos rng) 
                                }

    match (Exception rng str) = mkSe (render str) rng
    match PatternErr = SnippetError "Pattern error" []
    match (IOException rng ex) = mkSe (show ex) rng
    match (TypeError _ cls) =
      let rng = envRange $ clEnv cls in
      let msg = show $ clValue cls in
      mkSe msg rng
