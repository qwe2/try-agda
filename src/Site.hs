{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

import Api.Core
import Data.ByteString (ByteString)
import Snap.Snaplet
import Snap.Util.FileServe
import Application

routes :: [(ByteString, Handler App App ())]
routes = [("", serveDirectory "resources")]

app :: SnapletInit App App
app = makeSnaplet "app" "A snaplet example application." Nothing $ do
  iapi <- nestSnaplet "api" api apiInit
  addRoutes routes
  return $ App iapi
