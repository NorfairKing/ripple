module Server exposing (imgUrl, list, reRipple, upload)

import Coordinates exposing (Coordinates)
import Url.Builder exposing (relative, string)


list : Coordinates -> String
list coords =
    relative [ "list" ]
        [ string "latitude" <| String.fromFloat coords.latitude
        , string "longitude" <| String.fromFloat coords.longitude
        ]


imgUrl : String -> String
imgUrl imgID =
    relative [ "ripple", imgID ] []


upload : String
upload =
    relative [ "upload" ] []


reRipple : String
reRipple =
    relative [ "re-ripple" ] []
