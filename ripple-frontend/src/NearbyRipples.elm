module NearbyRipples exposing (Model, Msg, initCmd, initModel, update, view)

import Coordinates exposing (Coordinates)
import Html exposing (Html, div, img)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Material.Button as Button
import Material.Dialog as Dialog
import Material.ImageList as ImageList
import Material.ImageList.Item as ImageListItem exposing (ImageListItem)
import Maybe.Extra as Maybe
import Ripple exposing (Ripple)
import Server
import Url


type alias Model =
    { nearbyRipples : List Ripple
    , seenRipples : List Ripple
    , selectedRipple : Maybe Ripple
    }


initModel : Model
initModel =
    { nearbyRipples = []
    , seenRipples = []
    , selectedRipple = Nothing
    }


initCmd : Coordinates -> Cmd Msg
initCmd startLocation =
    getList GotRipples startLocation


type Msg
    = Refresh
    | GotRipples (Result Http.Error (List Ripple))
    | Deselect
    | Select Ripple
    | ReRipple Ripple
    | GotReRippleReply (Result Http.Error ())
    | UnRipple Ripple


update : Coordinates -> Msg -> Model -> ( Model, Cmd Msg )
update coords msg model =
    case msg of
        Refresh ->
            ( model, getList GotRipples coords )

        GotRipples errOrRipples ->
            case errOrRipples of
                Err _ ->
                    ( model, Cmd.none )

                Ok ripples ->
                    ( { model | nearbyRipples = ripples }
                    , Cmd.none
                    )

        Deselect ->
            ( { model | selectedRipple = Nothing }
            , Cmd.none
            )

        Select ripple ->
            ( { model | selectedRipple = Just ripple }
            , Cmd.none
            )

        ReRipple ripple ->
            ( markRippleAsSeen ripple { model | selectedRipple = Nothing }
            , reRipple GotReRippleReply coords ripple
            )

        GotReRippleReply _ ->
            ( model
            , Cmd.none
            )

        UnRipple ripple ->
            ( markRippleAsSeen ripple { model | selectedRipple = Nothing }
            , Cmd.none
            )


markRippleAsSeen : Ripple -> Model -> Model
markRippleAsSeen ripple model =
    let
        seenRipples =
            ripple :: model.seenRipples
    in
    { model | seenRipples = seenRipples }


unseenRipples : Model -> List Ripple
unseenRipples model =
    List.foldl List.remove model.nearbyRipples model.seenRipples


view : Model -> Html Msg
view model =
    div [] <|
        Maybe.values
            [ Just <|
                ImageList.imageList ImageList.config <|
                    List.map viewListElement (unseenRipples model)
            , Maybe.map viewOptionsDialog model.selectedRipple
            ]


viewListElement : Ripple -> ImageListItem Msg
viewListElement ripple =
    ImageListItem.imageListItem
        (ImageListItem.config
            |> ImageListItem.setAttributes
                [ style "width" "calc(100% / 2 - 4pt)"
                , style "margin" "2pt"
                , style "cursor" "pointer"
                , onClick <| Select ripple
                ]
        )
        (Url.toString <| Server.imgUrl ripple.id)


viewOptionsDialog : Ripple -> Html Msg
viewOptionsDialog ripple =
    Dialog.alert
        (Dialog.config
            |> Dialog.setOpen True
            |> Dialog.setOnClose Deselect
        )
        { content =
            [ img
                [ src <| Url.toString <| Server.imgUrl ripple.id
                , style "max-height" "100%"
                , style "max-width" "100%"
                ]
                []
            ]
        , actions =
            [ Button.text
                (Button.config |> Button.setOnClick Deselect)
                "Cancel"
            , Button.text
                (Button.config |> Button.setOnClick (UnRipple ripple))
                "Delete"
            , Button.text
                (Button.config
                    |> Button.setOnClick (ReRipple ripple)
                    |> Button.setAttributes [ Dialog.defaultAction ]
                )
                "Re-Ripple"
            ]
        }


getList : (Result Http.Error (List Ripple) -> msg) -> Coordinates -> Cmd msg
getList msg coords =
    Http.get
        { url = Server.list coords |> Url.toString
        , expect = Http.expectJson msg (Decode.list Ripple.decoder)
        }


reRipple : (Result Http.Error () -> msg) -> Coordinates -> Ripple -> Cmd msg
reRipple msg currentCoords ripple =
    Http.post
        { url = Server.reRipple |> Url.toString
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "coordinates", Coordinates.toJSON currentCoords )
                    , ( "id", Encode.string ripple.id )
                    ]
        , expect = Http.expectWhatever msg
        }
