{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | We store latitude and longitude as a fixed-point number so we don't have to deal with NaNs.
--
-- This is a much deeper rabit hole than I expected:
--
--  * A latitude and longitude have a qualitative difference.
--    A longitude can be considered to wrap around modularly but a latitude rather not.
--
--  * A latitude is in the range [-90, 90] where -90 is opposite to 90.
--
--  * A longitude is in the range [-180, 180[ where -180 is the same as 180, so we choose an open interval.
--
--  * The precision that we use matters.
--    We are using five digits of precision, which corresponds to about 1m.
--    This is more than enough to point at a party.
--    See https://xkcd.com/2170/ for more details.
--
--  We made these decisions:
--
--  * Num instance for literals, but for nothing else
--  * Real and Fractional instances to be able to convert to Double for distance calculations
--  * No Enum instances because we don't need them.
module Data.Coordinates
  ( Coord (..),
    fixedToCoord,
    coordToFixed,
    renderCoord,
    parseCoordOrError,
    parseCoordOrFail,
    Latitude (..),
    mkLatitude,
    mkLatitudeOrError,
    latitudeToFloat,
    latitudeToDouble,
    Longitude (..),
    mkLongitude,
    mkLongitudeOrError,
    longitudeToFloat,
    longitudeToDouble,
    Coordinates (..),
    distanceTo,
  )
where

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.Arrow (left)
import Control.DeepSeq
import Data.Aeson (FromJSON, ToJSON)
import Data.Fixed
import Data.GenValidity
import Data.Hashable
import Data.Int
import Data.List
import Data.OpenApi as OpenApi (ToSchema)
import Data.Proxy
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Test.QuickCheck (choose, suchThatMap)
import Text.Read
import Web.PathPieces

data E5

coordResolution :: Integer
coordResolution = 100_000

instance HasResolution E5 where
  resolution _ = coordResolution

fixedToCoord :: Fixed E5 -> Coord
fixedToCoord (MkFixed i) = Coord {unCoord = fromInteger i}

coordToFixed :: Coord -> Fixed E5
coordToFixed (Coord i) = MkFixed (fromIntegral i)

renderCoord :: Coord -> Text
renderCoord = T.pack . show . (realToFrac :: Coord -> Double)

parseCoordOrError :: Text -> Either String Coord
parseCoordOrError t = case readMaybe (T.unpack t) of
  Nothing -> Left $ "Un-Read-able Coord:" <> show t
  Just f -> pure $ (realToFrac :: Double -> Coord) f

parseCoordOrFail :: MonadFail m => Text -> m Coord
parseCoordOrFail t = case parseCoordOrError t of
  Left err -> fail err
  Right l -> pure l

-- Newtype for a custom 'PersistFieldSql' instance. (and to hide unused instances)
--
-- We store a coordinate as a fixed-point number with scale factor 10E5
newtype Coord = Coord {unCoord :: Int64}
  deriving stock (Eq, Ord, Generic)
  deriving (FromJSON, ToJSON, OpenApi.ToSchema) via (Autodocodec Coord)

instance Validity Coord

instance Hashable Coord

instance NFData Coord

instance Show Coord where
  show = show . coordToFixed

instance Read Coord where
  readPrec = fixedToCoord <$> readPrec

instance HasCodec Coord where
  codec = dimapCodec fixedToCoord coordToFixed (codecViaAeson "Fixed E5")

instance PersistField Coord where
  toPersistValue (Coord i) = toPersistValue i -- Store coordinates as integers so we don't lose precision
  fromPersistValue (PersistInt64 i) = pure $ Coord i
  fromPersistValue pv = fixedToCoord <$> fromPersistValue pv

instance PersistFieldSql Coord where
  sqlType Proxy = SqlInt64

instance Num Coord where
  fromInteger = Coord . fromInteger . (* coordResolution)
  (+) = binOpFixed (+)
  (-) = binOpFixed (-)
  (*) = binOpFixed (*)
  abs (Coord i) = Coord $ abs i
  signum (Coord i) = Coord $ signum i
  negate (Coord i) = Coord $ negate i

binOpFixed :: (Fixed E5 -> Fixed E5 -> Fixed E5) -> (Coord -> Coord -> Coord)
binOpFixed f c1 c2 = fixedToCoord $ coordToFixed c1 `f` coordToFixed c2

instance Real Coord where
  toRational (Coord i) = fromIntegral i % coordResolution

instance Fractional Coord where
  (/) = binOpFixed (/)
  fromRational = fixedToCoord . fromRational

instance GenValid Coord where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

newtype Latitude = Latitude {unLatitude :: Coord}
  deriving stock (Eq, Ord, Generic)
  deriving (FromJSON, ToJSON, OpenApi.ToSchema) via (Autodocodec Coord)

mkLatitude :: Coord -> Maybe Latitude
mkLatitude = constructValid . Latitude

mkLatitudeOrError :: Coord -> Either String Latitude
mkLatitudeOrError = prettyValidate . Latitude

mkLatitudeOrFail :: MonadFail m => Coord -> m Latitude
mkLatitudeOrFail n = case mkLatitudeOrError n of
  Left err -> fail err
  Right l -> pure l

instance Validity Latitude where
  validate lat@Latitude {..} =
    mconcat
      [ genericValidate lat,
        declare ("Is -90 or more: " <> show unLatitude) $ lat >= minBound,
        declare ("Is 90 or less: " <> show unLatitude) $ lat <= maxBound
      ]

instance Hashable Latitude

instance NFData Latitude

instance Show Latitude where
  show = show . unLatitude

instance Read Latitude where
  readPrec = readPrec >>= mkLatitudeOrFail

instance Bounded Latitude where
  minBound = Latitude (-90)
  maxBound = Latitude 90

instance HasCodec Latitude where
  codec = bimapCodec mkLatitudeOrError unLatitude codec

instance PersistField Latitude where
  toPersistValue = toPersistValue . unLatitude
  fromPersistValue pv = fromPersistValue pv >>= (left T.pack . mkLatitudeOrError)

instance PersistFieldSql Latitude where
  sqlType Proxy = sqlType (Proxy :: Proxy Coord)

latitudeToFloat :: RealFloat f => Latitude -> f
latitudeToFloat = realToFrac . unLatitude

-- | Type-constrained version of 'latitudeToFloat'
latitudeToDouble :: Latitude -> Double
latitudeToDouble = latitudeToFloat

instance GenValid Latitude where
  genValid =
    choose (-90_00000, 90_00000)
      `suchThatMap` (mkLatitude . fixedToCoord . MkFixed)
  shrinkValid = shrinkValidStructurally

newtype Longitude = Longitude {unLongitude :: Coord}
  deriving stock (Eq, Ord, Generic)
  deriving (FromJSON, ToJSON, OpenApi.ToSchema) via (Autodocodec Coord)

mkLongitude :: Coord -> Maybe Longitude
mkLongitude = constructValid . Longitude

mkLongitudeOrError :: Coord -> Either String Longitude
mkLongitudeOrError = prettyValidate . Longitude

mkLongitudeOrFail :: MonadFail m => Coord -> m Longitude
mkLongitudeOrFail n = case mkLongitudeOrError n of
  Left err -> fail err
  Right l -> pure l

instance Validity Longitude where
  validate lon@Longitude {..} =
    mconcat
      [ genericValidate lon,
        declare ("Is -180 or more: " <> show unLongitude) $ lon >= minBound,
        declare ("Is 180 or less: " <> show unLongitude) $ lon <= maxBound
      ]

instance Hashable Longitude

instance NFData Longitude

instance Show Longitude where
  show = show . unLongitude

instance Read Longitude where
  readPrec = readPrec >>= mkLongitudeOrFail

instance PersistField Longitude where
  toPersistValue = toPersistValue . unLongitude
  fromPersistValue pv = fromPersistValue pv >>= (left T.pack . mkLongitudeOrError)

instance PersistFieldSql Longitude where
  sqlType Proxy = sqlType (Proxy :: Proxy Coord)

instance Bounded Longitude where
  minBound = Longitude (-180)
  maxBound = Longitude (180 - Coord 1)

instance HasCodec Longitude where
  codec = bimapCodec mkLongitudeOrError unLongitude codec

longitudeToFloat :: RealFloat f => Longitude -> f
longitudeToFloat = realToFrac . unLongitude

-- | Type-constrained version of 'longitudeToFloat'
longitudeToDouble :: Longitude -> Double
longitudeToDouble = longitudeToFloat

instance GenValid Longitude where
  genValid =
    choose (-180_00000, 180_00000 - 1)
      `suchThatMap` (mkLongitude . fixedToCoord . MkFixed)
  shrinkValid = shrinkValidStructurally

data Coordinates = Coordinates
  { coordinatesLat :: !Latitude,
    coordinatesLon :: !Longitude
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON, OpenApi.ToSchema) via (Autodocodec Coordinates)

instance Validity Coordinates

instance Hashable Coordinates

instance NFData Coordinates

instance PathPiece Coordinates where
  toPathPiece :: Coordinates -> Text
  toPathPiece Coordinates {..} =
    T.pack $
      intercalate
        ","
        [ show coordinatesLat,
          show coordinatesLon
        ]

  fromPathPiece :: Text -> Maybe Coordinates
  fromPathPiece t = case T.split (== ',') t of
    [latText, lonText] ->
      Coordinates
        <$> readMaybe (T.unpack latText)
        <*> readMaybe (T.unpack lonText)
    _ -> Nothing

instance HasCodec Coordinates where
  codec =
    object "Coordinates" $
      Coordinates
        <$> requiredField "latitude" "latitude" .= coordinatesLat
        <*> requiredField "longitude" "longitude" .= coordinatesLon

instance GenValid Coordinates where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

-- See #https://en.wikipedia.org/wiki/Haversine_formula#Formulation
--
-- The resulting double will be much more precise than the actual distance, so watch out.
distanceTo ::
  Coordinates ->
  Coordinates ->
  -- | In metres
  Word
distanceTo co1 co2 =
  let toRadians = (* (pi / 180))
      lat1 = toRadians $ latitudeToFloat $ coordinatesLat co1 :: Double
      lat2 = toRadians $ latitudeToFloat $ coordinatesLat co2 :: Double
      lon1 = toRadians $ longitudeToFloat $ coordinatesLon co1 :: Double
      lon2 = toRadians $ longitudeToFloat $ coordinatesLon co2 :: Double
      latDiff = lat2 - lat1
      lonDiff = lon2 - lon1
      sinSqLat = sin (latDiff / 2) ^ (2 :: Int)
      sinSqLon = sin (lonDiff / 2) ^ (2 :: Int)
      under = sinSqLat + cos lat1 * cos lat2 * sinSqLon
      -- Average radius of earth:
      r = 6_371_000 -- m
   in round $ 2 * r * asin (sqrt under)
