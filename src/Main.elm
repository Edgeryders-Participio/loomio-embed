module Main exposing (..)

import Browser
import Html exposing (Html, text)
import Http

import Json.Decode as Json

import Url

import Loomio


type Msg
    = SetComments Model
    | GotDiscussion (Result Http.Error Loomio.DiscussionInfo)
    | NoOp

type alias Model = List Loomio.Comment

discussionUrl : String -> Url.Url


init : flags -> (Model, Cmd Msg)
init flags =
    ([]
    , Http.get
        { url = "https://talk.theborderland.se/api/v1/discussions/6HMxK2ve" -- from element
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
                    let
                        _ = Debug.log "discussion" discussionInfo
                    in
                        ( model
                        , Cmd.none )
                _ ->
                    ( model, Cmd.none )
    
            -- (model, Http.get
            --     { url = "https://talk.theborderland.se/api/v1/events?from=-10&per=20&order=sequence_id&discussion_id=965"
            --     , expect = Http.expectJson SetComments Loomio.decodeComments
            --     })
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
