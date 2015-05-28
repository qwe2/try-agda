{-# LANGUAGE ScopedTypeVariables #-}

module AgdaComm.TypeCheck (doCheck) where

import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Agda.Interaction.Imports        ( typeCheckMain, MaybeWarnings )
import Agda.Interaction.Options        ( defaultOptions )
import Agda.TypeChecking.Monad.Base    ( Interface, runTCMTop, TCErr(PatternErr, IOException, Exception, TypeError), envRange, clEnv, clValue )
import Agda.TypeChecking.Monad.Options ( setCommandLineOptions )
import Agda.Utils.FileName             ( absolute )

doCheck :: String -> IO (Maybe TCErr)
doCheck relative = do
  absol <- absolute relative

  r :: Either TCErr (Interface, MaybeWarnings) <- liftIO $ runTCMTop $
    do setCommandLineOptions defaultOptions
       typeCheckMain absol

  case r of
    Left tcerr ->
      return $ Just tcerr
    Right _ ->
      return Nothing
    {-
     -Left (Exception rng str) -> do
     -  print rng
     -  putStrLn str
     -Left PatternErr ->
     -  putStrLn "Pattern error"
     -Left (IOException rng ex) -> do
     -  putStrLn "IO Exception"
     -  print rng
     -  print ex
     -Left (TypeError _ cls) -> do
     -  putStrLn "Type error"
     -  print $ envRange $ clEnv cls
     -  print $ clValue cls
     -Right (i, _) ->
     -  print i
     -}

