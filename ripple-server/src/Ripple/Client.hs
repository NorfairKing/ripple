{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Ripple.Client where

import Ripple.API
import Servant.API
import Servant.Client

clientUploadRipple :: ClientM NoContent
clientListRipples :: ClientM [RippleSummary]
clientReRipple :: ClientM NoContent
clientUploadRipple
  :<|> clientListRipples
  :<|> clientReRipple = client rippleAPI
