{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Ripple.Client where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LB
import Data.Coordinates
import Ripple.API
import Servant.API
import Servant.Client
import Servant.Multipart.Client

clientUploadRippleRaw :: (LB.ByteString, UploadRippleRequest) -> ClientM RippleUuid
clientListRipples :: Coordinates -> ClientM [RippleSummary]
clientGetRipple :: RippleUuid -> ClientM RippleContent
clientReRipple :: ReRippleRequest -> ClientM RippleUuid
clientUploadRippleRaw
  :<|> clientListRipples
  :<|> clientGetRipple
  :<|> clientReRipple = client rippleAPI

clientUploadRipple :: UploadRippleRequest -> ClientM RippleUuid
clientUploadRipple upload = do
  boundary <- liftIO genBoundary
  clientUploadRippleRaw (boundary, upload)
