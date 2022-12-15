{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ripple.APISpec (spec) where

import Control.Lens
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.OpenApi
import Data.Proxy
import Data.Text (Text)
import Network.HTTP.Media (MediaType, (//))
import Ripple.API
import Ripple.DB
import Servant.API ((:>))
import Servant.Multipart
import Servant.OpenApi
import Servant.OpenApi.Internal
import Test.Syd
import Test.Syd.Aeson

-- Based on https://github.com/biocad/servant-openapi3/issues/19
instance (HasOpenApi sub) => HasOpenApi (MultipartForm tag UploadRippleRequest :> sub) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy sub) & addRequestBody reqBody
    where
      myMediaType :: MediaType
      myMediaType = "multipart" Network.HTTP.Media.// "form-data"
      myProperties :: InsOrdHashMap Text (Referenced Schema)
      myProperties =
        InsOrdHashMap.fromList
          [ ( "image",
              Inline
                ( mempty
                    { _schemaFormat = Just "binary"
                    }
                )
            )
          ]
      mySchema :: Referenced Schema
      mySchema =
        Inline
          ( mempty
              { _schemaType = Just OpenApiObject,
                _schemaProperties = myProperties
              }
          )
      reqBody :: RequestBody
      reqBody =
        mempty & content
          .~ InsOrdHashMap.fromList [(myMediaType, mempty & schema ?~ mySchema)]

spec :: Spec
spec = do
  it "outputs the openapi spec the same as before" $
    pureGoldenJSONValueFile "openapi.json" (toOpenApi (Proxy :: Proxy RippleAPI))
