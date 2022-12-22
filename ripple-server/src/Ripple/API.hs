{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.List.NonEmpty (NonEmpty (..))
import Data.OpenApi as OpenApi (NamedSchema (..), ToParamSchema (..), ToSchema (..), binarySchema)
import Data.Proxy
import Data.Text (Text)
import Data.Typeable
import GHC.Generics (Generic)
import qualified Network.HTTP.Media as M
import Ripple.DB
import Servant.API
import Servant.Multipart
import Servant.Multipart.API

completeAPI :: Proxy CompleteAPI
completeAPI = Proxy

type CompleteAPI =
  FrontendAPI
    :<|> RippleAPI

type FrontendAPI =
  IndexHtml
    :<|> AppJs
    :<|> StyleCSS
    :<|> SiteWebmanifest

data HTML deriving (Typeable)

-- | @text/html;charset=utf-8@
instance Accept HTML where
  contentTypes _ =
    "text" M.// "html" M./: ("charset", "utf-8")
      :| ["text" M.// "html"]

instance MimeRender HTML ByteString where
  mimeRender Proxy = LB.fromStrict

type IndexHtml = Get '[HTML] ByteString

data JS deriving (Typeable)

-- | @text/javascript
instance Accept JS where
  contentTypes _ = "text" M.// "javascript" :| []

instance MimeRender JS ByteString where
  mimeRender Proxy = LB.fromStrict

type AppJs = "app.js" :> Get '[JS] ByteString

data CSS deriving (Typeable)

-- | @text/css
instance Accept CSS where
  contentTypes _ = "text" M.// "css" :| []

instance MimeRender CSS ByteString where
  mimeRender Proxy = LB.fromStrict

type StyleCSS = "style.css" :> Get '[CSS] ByteString

data Webmanifest deriving (Typeable)

-- | @application/manifest+json
instance Accept Webmanifest where
  contentTypes _ = "application" M.// "manifest+json" :| []

instance MimeRender Webmanifest ByteString where
  mimeRender Proxy = LB.fromStrict

type SiteWebmanifest = "site.webmanifest" :> Get '[Webmanifest] ByteString

rippleAPI :: Proxy RippleAPI
rippleAPI = Proxy

-- | A separate API for only the parts we contact from the frontend
-- The home route can't be in here because there is no MimeUnrender for html:
-- https://github.com/haskell-servant/servant-blaze/issues/15
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
    :> QueryParam "latitude" Latitude
    :> QueryParam "longitude" Longitude
    :> Get '[JSON] [RippleSummary]

data RippleSummary = RippleSummary
  { rippleSummaryId :: RippleUuid,
    rippleSummaryCoordinates :: Coordinates
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, OpenApi.ToSchema) via (Autodocodec RippleSummary)

instance HasCodec RippleSummary where
  codec =
    object "RippleSummary" $
      RippleSummary
        <$> requiredField "id" "identifier" .= rippleSummaryId
        <*> requiredField "coordinates" "coordinates" .= rippleSummaryCoordinates

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
    :> Post '[JSON] RippleUuid

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

instance OpenApi.ToParamSchema Coord where
  toParamSchema Proxy = toParamSchema (Proxy :: Proxy Double)

instance OpenApi.ToParamSchema Longitude

instance OpenApi.ToParamSchema Latitude
