{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Ripple.Client where

import Data.Coordinates
import Ripple.API
import Servant.API
import Servant.Client

clientUploadRipple :: Coordinates -> ClientM NoContent
clientListRipples :: Coordinates -> ClientM [RippleSummary]
clientReRipple :: Coordinates -> ClientM NoContent
clientUploadRipple
  :<|> clientListRipples
  :<|> clientReRipple = client rippleAPI
