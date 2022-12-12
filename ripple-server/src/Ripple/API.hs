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
import Data.GenValidity.UUID.Typed ()
import Data.List
import Data.OpenApi as OpenApi (ToSchema)
import Data.Proxy
import Data.Text (Text)
import Data.UUID.Typed
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
    :> MultipartForm Mem UploadRippleRequest
    :> PostNoContent

data UploadRippleRequest = UploadRippleRequest
  { uploadRippleRequestFileType :: Text,
    uploadRippleRequestFileContents :: ByteString,
    uploadRippleRequestCoordinates :: Coordinates
  }
  deriving (Show, Eq, Generic)

instance Validity UploadRippleRequest

instance FromMultipart Mem UploadRippleRequest where
  fromMultipart MultipartData {..} = do
    let requireInput key = case find ((== key) . iName) inputs of
          Nothing -> Left $ "Key not found: " <> show key
          Just val -> pure (iValue val)
    latVal <- requireInput "latitude"
    lonVal <- requireInput "longitude"
    coordinatesLat <- parseCoordOrError latVal >>= mkLatitudeOrError
    coordinatesLon <- parseCoordOrError lonVal >>= mkLongitudeOrError
    let uploadRippleRequestCoordinates = Coordinates {..}
    (uploadRippleRequestFileType, uploadRippleRequestFileContents) <- case files of
      [FileData {..}] -> case fdInputName of
        "image" -> pure (fdFileCType, LB.toStrict fdPayload)
        _ -> Left $ "Incorrect file input name: " <> show fdInputName
      [] -> Left "Missing file"
      _ -> Left "Too many files."
    pure UploadRippleRequest {..}

instance ToMultipart Mem UploadRippleRequest where
  toMultipart UploadRippleRequest {..} =
    let Coordinates {..} = uploadRippleRequestCoordinates
        latInput = Input "latitude" $ renderCoord $ unLatitude coordinatesLat
        lonInput = Input "longitude" $ renderCoord $ unLongitude coordinatesLon
        inputs = [latInput, lonInput]
        file =
          FileData
            { fdInputName = "image",
              fdFileName = "image.jpg",
              fdFileCType = "image/jpeg",
              fdPayload = LB.fromStrict uploadRippleRequestFileContents
            }
        files = [file]
     in MultipartData {..}

instance GenValid UploadRippleRequest

type ListRipples =
  "list"
    :> ReqBody '[JSON] Coordinates
    :> Get '[JSON] [RippleSummary]

data RippleSummary = RippleSummary
  { rippleSummaryId :: RippleUuid
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, OpenApi.ToSchema) via (Autodocodec RippleSummary)

instance HasCodec RippleSummary where
  codec =
    object "RippleSummary" $
      RippleSummary <$> requiredField "id" "identifier" .= rippleSummaryId

type ReRipple =
  "re-ripple"
    :> ReqBody '[JSON] ReRippleRequest
    :> PostNoContent

data ReRippleRequest = ReRippleRequest
  { reRippleRequestCoordinates :: Coordinates,
    reRippleRequestId :: RippleUuid
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, OpenApi.ToSchema) via (Autodocodec ReRippleRequest)

instance HasCodec ReRippleRequest where
  codec =
    object "ReRippleRequest" $
      ReRippleRequest
        <$> requiredField "coordinates" "coordinates from where to re-ripple" .= reRippleRequestCoordinates
        <*> requiredField "id" "identifier of the ripple" .= reRippleRequestId

type RippleUuid = UUID RippleSummary
