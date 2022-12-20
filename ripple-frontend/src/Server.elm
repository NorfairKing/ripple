module Server exposing (imgUrl, list, reRipple, upload)

import Coordinates exposing (Coordinates)
import Url exposing (Url)


baseUrl : Url
baseUrl =
    -- { protocol = Url.Https
    -- , host = "ripple.cs-syd.eu"
    -- , port_ = Nothing
    -- , path = ""
    -- , query = Nothing
    -- , fragment = Nothing
    -- }
    { protocol = Url.Http
    , host = "localhost"
    , port_ = Just 9701
    , path = ""
    , query = Nothing
    , fragment = Nothing
    }


list : Coordinates -> Url
list coords =
    { baseUrl
        | path = "/list"
        , query =
            Just <|
                "latitude="
                    ++ String.fromFloat coords.latitude
                    ++ "&longitude="
                    ++ String.fromFloat coords.longitude
    }


imgUrl : String -> Url
imgUrl imgID =
    { baseUrl | path = "/ripple/" ++ imgID }


upload : Url
upload =
    { baseUrl | path = "/upload" }


reRipple : Url
reRipple =
    { baseUrl | path = "/re-ripple" }
