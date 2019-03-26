module Main exposing (..)

import Browser
import Html exposing (Html, text)


type Msg
    = NoOp


type alias Model =
    {}

init : flags -> (Model, Cmd msg)
init flags = ({}, Cmd.none)

view : Model -> Html Msg
view model =
    text "asd"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
