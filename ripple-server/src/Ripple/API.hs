{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
import Data.OpenApi as OpenApi (NamedSchema (..), ToParamSchema, ToSchema (..), binarySchema)
import Data.Proxy
import Data.Text (Text)
import Data.Typeable
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
    :<|> GetRipple
    :<|> ReRipple

type UploadRipple =
  "upload"
    :> MultipartForm Mem UploadRippleRequest
    :> Post '[JSON] RippleUuid

data UploadRippleRequest = UploadRippleRequest
  { uploadRippleRequestFileType :: Text,
    uploadRippleRequestFileContents :: RippleContent,
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
        "image" -> pure (fdFileCType, RippleContent {unRippleContent = LB.toStrict fdPayload})
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
              fdPayload = LB.fromStrict $ unRippleContent uploadRippleRequestFileContents
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

type GetRipple =
  "ripple"
    :> Capture "uuid" RippleUuid
    :> Get '[OctetStream] RippleContent

newtype RippleContent = RippleContent {unRippleContent :: ByteString}
  deriving (Show, Eq, Generic)

instance Validity RippleContent

instance OpenApi.ToSchema RippleContent where
  declareNamedSchema Proxy = do
    pure $ NamedSchema (Just "RippleContent") binarySchema

instance MimeRender OctetStream RippleContent where
  mimeRender Proxy RippleContent {..} = mimeRender (Proxy :: Proxy OctetStream) unRippleContent

instance MimeUnrender OctetStream RippleContent where
  mimeUnrenderWithType Proxy mt bs = do
    b <- mimeUnrenderWithType (Proxy :: Proxy OctetStream) mt bs
    pure RippleContent {unRippleContent = b}

instance GenValid RippleContent

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

deriving via (Autodocodec RippleUuid) instance Typeable tag => OpenApi.ToSchema (UUID tag)

instance Typeable tag => OpenApi.ToParamSchema (UUID tag)
