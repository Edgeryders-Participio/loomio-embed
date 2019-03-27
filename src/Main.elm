module Main exposing (..)

import Browser
import Html exposing (Html, text)
import Html.Attributes as Attrs
import Http

import Json.Decode as Json

import Url

import Loomio

-- TODO
-- Make pretty
-- Fix avatars (gravatar / url)

-- Construct URL
-- Get element attributes
-- Link to comment field in loomio
-- Match @username and make <strong>

baseUrl =
    { protocol = Url.Protocol.Http
    , host = "talk.theborderland.se"
    , port_ = Nothing
    , path = "/"
    , query = Nothing
    , fragment = Nothing
    }

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
    Html.div []
        [ Html.node "link" [ Attrs.rel "stylesheet", Attrs.href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" ] [] -- for testing
        , Loomio.viewComments model
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDiscussion result ->
            case result of
                Ok discussionInfo ->
                    (model, Http.get
                         { url = "http://talk.theborderland.se/api/v1/events?from=-10&per=20&order=sequence_id&discussion_id=965"
                         , expect = Http.expectJson GotComments (Loomio.decodeComments baseUrl)
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
