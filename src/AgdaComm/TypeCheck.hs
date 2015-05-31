{-# LANGUAGE ScopedTypeVariables #-}

module AgdaComm.TypeCheck (doCheck) where

import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Agda.Interaction.Imports        ( typeCheckMain, MaybeWarnings )
import Agda.Interaction.Options        ( defaultOptions, optIncludeDirs )
import Agda.TypeChecking.Monad.Base    ( Interface, runTCMTop, runTCMTop', TCErr(PatternErr, IOException, Exception, TypeError), envRange, clEnv )
import Agda.TypeChecking.Monad.Options ( setCommandLineOptions )
import Agda.TypeChecking.Errors        ( prettyError )
import Agda.Utils.FileName             ( absolute )
import System.FilePath.Posix

import Type.SnippetError

libPath :: FilePath
libPath = "agda-stdlib-0.9/src/"

doCheck :: String -> IO SnippetError
doCheck relative = do
  let dir = takeDirectory relative
  absol <- absolute relative

  r :: Either TCErr (Interface, MaybeWarnings) <- liftIO $ runTCMTop $
    do setCommandLineOptions (defaultOptions { optIncludeDirs = Left [".", dir, libPath] })
       typeCheckMain absol

  case r of
    Left tcerr -> do
      msg <- runTCMTop' $ prettyError tcerr
      return $ mkSe (stripError msg) (match tcerr)
    Right _ ->
      return Ok
  where
    mkSe err rng = SnippetError { errorMessage = err
                                , ranges = rng
                                }

    stripError ('/' : rest) = tail $ dropWhile ('\n' /=) rest -- starts with filename
    stripError xs = xs

    match (Exception rng _) = rngToPos rng
    match PatternErr = []
    match (IOException rng _) = rngToPos rng
    match (TypeError _ cls) =
      let rng = envRange $ clEnv cls in
      rngToPos rng
