port module Ports exposing (..)

import Coordinates exposing (Coordinates)


{-| watchPosition matches the javascript name of the function
-}
port watchPosition : (Coordinates -> msg) -> Sub msg
