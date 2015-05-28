module Api.Post (resource) where

import Agda.TypeChecking.Monad.Base    (TCErr(PatternErr, IOException, Exception, TypeError), envRange, clEnv, clValue)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Text
import Data.Maybe
import qualified Data.Text.IO as D
import Data.UUID
import System.Random

import Rest
import qualified Rest.Resource as R

import Type.SnippetError
import Type.Snippet
import AgdaComm.TypeCheck

type Title = String

resource :: Resource IO (ReaderT Title IO) Title () Void
resource = mkResourceReader
  { R.name   = "post"
  , R.schema = withListing () $ named [("title", singleBy id)]
  , R.create = Just post
  }

putInFile :: Text -> IO String
putInFile content = do
  uuid <- randomIO
  let file = "tmp/" ++ toString uuid ++ ".agda"
  D.writeFile file content
  return file

{-
 -post :: Handler (ReaderT Snippet IO)
 -}
post = mkInputHandler (jsonO . jsonI) handler
  where
    mkSe err rng = SnippetError { errorMessage = err, ranges = fmap (\(s, p) -> (pack `fmap` s, p)) (rngToPos rng) }

    handler :: Snippet -> ExceptT Reason_ IO SnippetError
    handler snipp = do
      file <- liftIO $ putInFile $ code snipp
      err <- liftIO $ doCheck file
      return $ Ok `fromMaybe` (match `fmap` err)

    match x =
      case x of
        Exception rng str -> mkSe str rng
        PatternErr -> SnippetError "Pattern error" []
        IOException rng ex -> mkSe (show ex) rng
        TypeError _ cls ->
          let rng = envRange $ clEnv cls in
          let msg = show $ clValue cls in
          mkSe msg rng


