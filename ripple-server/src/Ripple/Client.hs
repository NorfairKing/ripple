{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module Ripple.Client where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LB
import Data.Coordinates
import Ripple.API
import Ripple.DB
import Servant.API
import Servant.Client
import Servant.Multipart.Client

clientUploadRippleRaw :: (LB.ByteString, UploadRippleRequest) -> ClientM RippleUuid
clientListRipplesRaw :: Maybe Latitude -> Maybe Longitude -> ClientM [RippleSummary]
clientGetRipple :: RippleUuid -> ClientM RippleContent
clientReRipple :: ReRippleRequest -> ClientM RippleUuid
clientUploadRippleRaw
  :<|> clientListRipplesRaw
  :<|> clientGetRipple
  :<|> clientReRipple = client rippleAPI

clientUploadRipple :: UploadRippleRequest -> ClientM RippleUuid
clientUploadRipple upload = do
  boundary <- liftIO genBoundary
  clientUploadRippleRaw (boundary, upload)

clientListRipples :: Coordinates -> ClientM [RippleSummary]
clientListRipples Coordinates {..} = clientListRipplesRaw (Just coordinatesLat) (Just coordinatesLon)
