{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Ripple.API where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Coordinates
import Data.OpenApi as OpenApi (ToSchema)
import Data.Proxy
import Servant.API
import Servant.Multipart

rippleAPI :: Proxy RippleAPI
rippleAPI = Proxy

type RippleAPI =
  UploadRipple
    :<|> ListRipples
    :<|> ReRipple

type UploadRipple =
  "upload"
    :> MultipartForm Tmp (MultipartData Tmp)
    :> PostNoContent

type ListRipples =
  "list"
    :> ReqBody '[JSON] Coordinates
    :> Get '[JSON] [RippleSummary]

type ReRipple =
  "re-ripple"
    :> ReqBody '[JSON] Coordinates
    :> PostNoContent

data RippleSummary = RippleSummary
  deriving (FromJSON, ToJSON, OpenApi.ToSchema) via (Autodocodec RippleSummary)

instance HasCodec RippleSummary where
  codec =
    object "RippleSummary" $
      pure RippleSummary
