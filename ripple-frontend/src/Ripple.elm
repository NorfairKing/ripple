module Ripple exposing (Ripple, decoder)

import Coordinates exposing (Coordinates)
import Json.Decode as Decode exposing (Decoder)


type alias RippleID =
    String


type alias Ripple =
    { id : RippleID
    , coordinates : Coordinates
    }


decoder : Decoder Ripple
decoder =
    Decode.map2 Ripple
        (Decode.field "id" Decode.string)
        (Decode.field "coordinates" Coordinates.decoder)
