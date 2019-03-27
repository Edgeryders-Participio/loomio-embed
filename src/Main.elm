module Main exposing (..)

import Browser
import Html exposing (Html, text)
import Http

import Json.Decode as Json

import Url

import Loomio


type Msg
    = GotComments (Result Http.Error (List Loomio.Comment))
    | GotDiscussion (Result Http.Error Loomio.DiscussionInfo)
    | NoOp

type alias Model = List Loomio.Comment

-- discussionUrl : String -> Url.Url
-- "https://talk.theborderland.se/api/v1/events?from=-10&per=20&order=sequence_id&discussion_id=965"

init : flags -> (Model, Cmd Msg)
init flags =
    ([]
    , Http.get
        { url = "http://talk.theborderland.se/api/v1/discussions/6HMxK2ve" -- from element
        , expect =
            Http.expectJson
                GotDiscussion
                ( Json.field "discussions"
                <| Json.index 0
                <| Loomio.decodeDiscussion
                )
        })


view : Model -> Html Msg
view model =
    text (Debug.toString model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDiscussion result ->
            case result of
                Ok discussionInfo ->
                    (model, Http.get
                         { url = "http://talk.theborderland.se/api/v1/events?from=-10&per=20&order=sequence_id&discussion_id=965"
                         , expect = Http.expectJson GotComments Loomio.decodeComments
                         })
                _ ->
                    ( model, Cmd.none )
        GotComments result ->
                (  Result.withDefault model (result |> Result.mapError (Debug.log "asd")), Cmd.none )
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
