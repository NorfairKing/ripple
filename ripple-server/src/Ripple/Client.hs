{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Ripple.Client where

import qualified Data.ByteString.Lazy as LB
import Data.Coordinates
import Ripple.API
import Servant.API
import Servant.Client
import Servant.Multipart
import Servant.Multipart.Client ()

clientUploadRipple :: Coordinates -> (LB.ByteString, MultipartData Tmp) -> ClientM NoContent
clientListRipples :: Coordinates -> ClientM [RippleSummary]
clientReRipple :: Coordinates -> ClientM NoContent
clientUploadRipple
  :<|> clientListRipples
  :<|> clientReRipple = client rippleAPI
