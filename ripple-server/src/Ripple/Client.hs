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

clientUploadRippleRaw :: (LB.ByteString, RippleUpload) -> ClientM NoContent
clientListRipples :: Coordinates -> ClientM [RippleSummary]
clientReRipple :: Coordinates -> ClientM NoContent
clientUploadRippleRaw
  :<|> clientListRipples
  :<|> clientReRipple = client rippleAPI

clientUploadRipple :: RippleUpload -> ClientM NoContent
clientUploadRipple upload = do
  boundary <- liftIO genBoundary
  clientUploadRippleRaw (boundary, upload)
