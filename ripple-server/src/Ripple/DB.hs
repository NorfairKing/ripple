{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-dodgy-exports -Wno-unused-pattern-binds #-}

module Ripple.DB where

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.DeepSeq
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Coordinates
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Persist ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID.Typed ()
import Data.OpenApi as OpenApi (ToParamSchema (..), ToSchema (..))
import Data.Proxy
import Data.Text (Text)
import Data.Time
import Data.Typeable
import qualified Data.UUID as UUID
import Data.UUID.Typed as Typed
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)
import Web.PathPieces

data R

type RippleUuid = UUID R

-- When adding a table here, be sure to add the corresponding roundtrip test as well.
share
  [mkPersist sqlSettings, mkMigrate "automaticMigrations"]
  [persistLowerCase|

Ripple sql=ripple
    -- UUID, for external usage
    uuid RippleUuid

    type Text
    contents ByteString

    latitude Latitude
    longitude Longitude

    original RippleUuid Maybe -- The original ripple, if it's a re-ripple
    parent RippleUuid Maybe -- The parent ripple, if it's a re-ripple

    created UTCTime

    UniqueRippleUuid uuid

    deriving Show
    deriving Eq
    deriving Generic
|]

instance (ToBackendKey SqlBackend record) => NFData (Key record) where
  rnf = rnf . fromSqlKey

instance (ToBackendKey SqlBackend record, NFData record) => NFData (Entity record) where
  rnf (Entity k v) = deepseq k $ deepseq v ()

instance Validity Ripple

instance NFData Ripple

instance GenValid Ripple

deriving via (Autodocodec (Typed.UUID tag)) instance (Typeable tag, HasCodec (Typed.UUID tag)) => OpenApi.ToSchema (Typed.UUID tag)

instance Typeable tag => OpenApi.ToParamSchema (Typed.UUID tag)

instance PersistField (UUID a) where
  toPersistValue (UUID uuid) = PersistByteString $ LB.toStrict $ UUID.toByteString uuid
  fromPersistValue pv = do
    bs <- fromPersistValue pv
    case UUID.fromByteString $ LB.fromStrict bs of
      Nothing -> Left "Invalid Bytestring to convert to UUID"
      Just uuid -> Right $ UUID uuid

instance PersistFieldSql (UUID a) where
  sqlType Proxy = SqlBlob

instance PathPiece (UUID a) where
  fromPathPiece = parseUUIDText
  toPathPiece = uuidText

rippleCoordinates :: Ripple -> Coordinates
rippleCoordinates Ripple {..} =
  let coordinatesLat = rippleLatitude
      coordinatesLon = rippleLongitude
   in Coordinates {..}
