{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Core where

import Control.Lens
import Snap.Snaplet
import qualified Data.ByteString.Char8 as B
import Api.Services.AgdaService

data Api = Api { _agdaService :: Snaplet AgdaService }

makeLenses ''Api

apiRoutes :: [(B.ByteString, Handler b Api ())]
apiRoutes = []

apiInit :: SnapletInit b Api
apiInit = makeSnaplet  "api" "Core API" Nothing $ do
  iapi <- nestSnaplet "agda" agdaService agdaServiceInit
  addRoutes apiRoutes
  return $ Api iapi
