{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Ripple.API where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Coordinates
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import Data.List
import Data.OpenApi as OpenApi (ToSchema)
import Data.Proxy
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API
import Servant.Multipart
import Servant.Multipart.API

rippleAPI :: Proxy RippleAPI
rippleAPI = Proxy

type RippleAPI =
  UploadRipple
    :<|> ListRipples
    :<|> ReRipple

type UploadRipple =
  "upload"
    :> MultipartForm Mem RippleUpload
    :> PostNoContent

data RippleUpload = RippleUpload
  { rippleUploadFileType :: Text,
    rippleUploadFileContents :: ByteString,
    rippleUploadCoordinates :: Coordinates
  }
  deriving (Show, Eq, Generic)

instance Validity RippleUpload

instance FromMultipart Mem RippleUpload where
  fromMultipart MultipartData {..} = do
    let requireInput key = case find ((== key) . iName) inputs of
          Nothing -> Left $ "Key not found: " <> show key
          Just val -> pure (iValue val)
    latVal <- requireInput "latitude"
    lonVal <- requireInput "longitude"
    coordinatesLat <- parseCoordOrError latVal >>= mkLatitudeOrError
    coordinatesLon <- parseCoordOrError lonVal >>= mkLongitudeOrError
    let rippleUploadCoordinates = Coordinates {..}
    (rippleUploadFileType, rippleUploadFileContents) <- case files of
      [FileData {..}] -> case fdInputName of
        "image" -> pure (fdFileCType, LB.toStrict fdPayload)
        _ -> Left $ "Incorrect file input name: " <> show fdInputName
      [] -> Left "Missing file"
      _ -> Left "Too many files."
    pure RippleUpload {..}

instance ToMultipart Mem RippleUpload where
  toMultipart RippleUpload {..} =
    let Coordinates {..} = rippleUploadCoordinates
        latInput = Input "latitude" $ renderCoord $ unLatitude coordinatesLat
        lonInput = Input "longitude" $ renderCoord $ unLongitude coordinatesLon
        inputs = [latInput, lonInput]
        file =
          FileData
            { fdInputName = "image",
              fdFileName = "image.jpg",
              fdFileCType = "image/jpeg",
              fdPayload = LB.fromStrict rippleUploadFileContents
            }
        files = [file]
     in MultipartData {..}

instance GenValid RippleUpload

type ListRipples =
  "list"
    :> ReqBody '[JSON] Coordinates
    :> Get '[JSON] [RippleSummary]

type ReRipple =
  "re-ripple"
    :> ReqBody '[JSON] Coordinates
    :> PostNoContent

data RippleSummary = RippleSummary
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, OpenApi.ToSchema) via (Autodocodec RippleSummary)

instance HasCodec RippleSummary where
  codec =
    object "RippleSummary" $
      pure RippleSummary
