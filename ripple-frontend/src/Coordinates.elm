module Coordinates exposing (Coordinates, decoder, distance, toJSON, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Latitude =
    Float


type alias Longitude =
    Float


type alias Coordinates =
    { latitude : Latitude
    , longitude : Longitude
    }


decoder : Decoder Coordinates
decoder =
    Decode.map2 Coordinates
        (Decode.field "latitude" Decode.float)
        (Decode.field "longitude" Decode.float)


toJSON : Coordinates -> Encode.Value
toJSON coords =
    Encode.object
        [ ( "latitude", Encode.float coords.latitude )
        , ( "longitude", Encode.float coords.longitude )
        ]


toString : Coordinates -> String
toString =
    Encode.encode 0 << toJSON


type alias Kilometers =
    Float


{-| Calculates the haversine distance <https://www.movable-type.co.uk/scripts/latlong.html>
-}
distance : Coordinates -> Coordinates -> Kilometers
distance coords1 coords2 =
    let
        lat1 =
            coords1.latitude

        lat2 =
            coords2.latitude

        lon1 =
            coords1.longitude

        lon2 =
            coords2.longitude

        r =
            6371

        φ1 =
            lat1 * pi / 180

        φ2 =
            lat2 * pi / 180

        dφ =
            (lat2 - lat1) * pi / 180

        dλ =
            (lon2 - lon1) * pi / 180

        a =
            sin (dφ / 2)
                * sin (dφ / 2)
                + cos φ1
                * cos φ2
                * sin (dλ / 2)
                * sin (dλ / 2)

        c =
            2 * atan2 (sqrt a) (sqrt (1 - a))
    in
    r * c
