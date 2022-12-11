{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Ripple.API where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Coordinates
import Data.Proxy
import Servant.API

rippleAPI :: Proxy RippleAPI
rippleAPI = Proxy

type RippleAPI =
  UploadRipple
    :<|> ListRipples
    :<|> ReRipple

type UploadRipple = ReqBody '[JSON] Coordinates :> PostNoContent

type ListRipples = ReqBody '[JSON] Coordinates :> Get '[JSON] [RippleSummary]

type ReRipple = ReqBody '[JSON] Coordinates :> PostNoContent

data RippleSummary = RippleSummary
  deriving (FromJSON, ToJSON) via (Autodocodec RippleSummary)

instance HasCodec RippleSummary where
  codec =
    object "RippleSummary" $
      pure RippleSummary
