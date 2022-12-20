module Main exposing (..)

import Browser
import Coordinates exposing (Coordinates)
import CreateRipple
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import NearbyRipples
import Ports exposing (watchPosition)


type alias Model =
    { location : Coordinates
    , createRipple : CreateRipple.Model
    , nearbyRipples : NearbyRipples.Model
    }


type Msg
    = GotLocation Coordinates
    | StartCreatingRipple
    | CreateRippleMsg CreateRipple.Msg
    | NearbyRipplesMsg NearbyRipples.Msg


type alias Flags =
    { startLocation : Coordinates
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { location = flags.startLocation
      , createRipple = CreateRipple.initModel
      , nearbyRipples = NearbyRipples.initModel
      }
    , Cmd.map NearbyRipplesMsg <| NearbyRipples.initCmd flags.startLocation
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    watchPosition GotLocation


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotLocation location ->
            ( { model | location = location }
            , Cmd.none
            )

        StartCreatingRipple ->
            ( model
            , Cmd.none
            )

        CreateRippleMsg msg ->
            let
                ( newModel, cmd ) =
                    CreateRipple.update model.location msg model.createRipple
            in
            ( { model | createRipple = newModel }
            , Cmd.map CreateRippleMsg cmd
            )

        NearbyRipplesMsg msg ->
            let
                ( newModel, cmd ) =
                    NearbyRipples.update model.location msg model.nearbyRipples
            in
            ( { model | nearbyRipples = newModel }
            , Cmd.map NearbyRipplesMsg cmd
            )


view : Model -> Html Msg
view model =
    div
        [ class "app" ]
        [ Html.map NearbyRipplesMsg <| NearbyRipples.view model.nearbyRipples
        , Html.map CreateRippleMsg <| CreateRipple.view model.createRipple
        ]
