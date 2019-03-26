module Main exposing (..)

import Browser
import Html exposing (Html, text)
import Http

import Loomio


type Msg
    = SetDiscussion Model
    | NoOp

type alias Model = Loomio.Discussion

init : flags -> (Model, Cmd msg)
init flags =
    ({}
    , Http.get
        { url = "jkhasd"
        , expect = Http.expectJson SetDiscussion Loomio.decodeDiscussion
        })

view : Model -> Html Msg
view model =
    text "asd"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetDiscussion newModel ->
            ( newModel, Cmd.none )

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
