module Main exposing (..)

import Browser
import Html exposing (Html, text)
import Http

import Loomio


type Msg
    = SetComments Model
    | GetComments Model Discussion
    | NoOp

type alias Model = List Loomio.Comment

init : flags -> (Model, Cmd msg)
init flags =
    ({}
    , Http.get
        { url = "https://talk.theborderland.se/api/v1/discussions/6HMxK2ve" -- from element
        , expect = Http.expectJson GetComments Loomio.decodeDiscussion
        })


view : Model -> Html Msg
view model =
    text "asd"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetComments discussion ->
            (model, Http.get
                { url = "https://talk.theborderland.se/api/v1/events?from=-10&per=20&order=sequence_id&discussion_id=965"
                , expect = Http.expectJson SetComments Loomio.decodeComments
                })
        SetComments newModel ->
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
