module CreateRipple exposing (Model, Msg, initModel, update, view)

import Coordinates exposing (Coordinates)
import File exposing (File)
import Html exposing (Html, div, img, input, label)
import Html.Attributes exposing (accept, attribute, class, hidden, name, src, style, type_)
import Html.Events exposing (on)
import Http
import Json.Decode as D
import Material.Button as Button
import Material.Dialog as Dialog
import Material.Fab as Fab
import Maybe.Extra as Maybe
import Server
import Task
import Url


type alias ImagePreviewUrl =
    String


type Model
    = Closed
    | LoadingPreview File
    | Ready File ImagePreviewUrl


initModel : Model
initModel =
    Closed


type Msg
    = Reset
    | SetImage File
    | LoadedImagePreview File ImagePreviewUrl
    | Upload File
    | UploadDone (Result Http.Error ())


update : Coordinates -> Msg -> Model -> ( Model, Cmd Msg )
update location msg model =
    case msg of
        Reset ->
            ( Closed
            , Cmd.none
            )

        SetImage image ->
            ( LoadingPreview image
            , Task.perform (LoadedImagePreview image) (File.toUrl image)
            )

        LoadedImagePreview image preview ->
            ( Ready image preview
            , Cmd.none
            )

        Upload image ->
            ( Closed
            , Http.post
                { url = Url.toString Server.upload
                , body =
                    Http.multipartBody
                        [ Http.stringPart "latitude" <| String.fromFloat location.latitude
                        , Http.stringPart "longitude" <| String.fromFloat location.longitude
                        , Http.filePart "image" image
                        ]
                , expect = Http.expectWhatever UploadDone
                }
            )

        UploadDone _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "createRipple" ] <|
        Maybe.values
            [ Just <| viewCreateButton Capture
            , case model of
                Closed ->
                    Nothing

                LoadingPreview _ ->
                    Nothing

                Ready file preview ->
                    Just <| viewCreateDialog file preview
            ]


type RippleMethod
    = Select
    | Capture


viewCreateButton : RippleMethod -> Html Msg
viewCreateButton rippleMethod =
    label [ class "button" ] <|
        let
            baseAttrs =
                [ type_ "file"
                , accept "image/*"
                , on "change" <| D.map SetImage inputFileDecoder
                , name "image"
                , hidden True
                ]
        in
        [ input
            (case rippleMethod of
                Select ->
                    -- Opening a selection dialog is the default, so add nothing
                    baseAttrs

                Capture ->
                    -- To capture directly from the camera add the "capture" attribute
                    attribute "capture" "environment" :: baseAttrs
            )
            []
        , Fab.fab
            (Fab.config
                |> Fab.setAttributes
                    [ style "position" "fixed"
                    , style "bottom" "2rem"
                    , style "right" "2rem"
                    ]
            )
            (Fab.icon "add")
        ]


inputFileDecoder : D.Decoder File
inputFileDecoder =
    D.at [ "target", "files" ] (D.oneOrMore (\first _ -> first) File.decoder)


viewCreateDialog : File -> ImagePreviewUrl -> Html Msg
viewCreateDialog file preview =
    Dialog.alert
        (Dialog.config
            |> Dialog.setOpen True
            |> Dialog.setOnClose Reset
        )
        { content =
            [ img
                [ src preview
                , style "max-height" "100%"
                , style "max-width" "100%"
                ]
                []
            ]
        , actions =
            [ Button.text
                (Button.config |> Button.setOnClick Reset)
                "Cancel"
            , Button.text
                (Button.config
                    |> Button.setOnClick (Upload file)
                    |> Button.setAttributes [ Dialog.defaultAction ]
                )
                "Share"
            ]
        }
