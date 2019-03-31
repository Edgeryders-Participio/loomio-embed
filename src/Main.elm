module Main exposing (Model, Msg(..), State, init, initialModelDecoder, main, subscriptions, update, view, viewComment, viewComments, viewUser)

import Browser
import Html exposing (Html, div, img, li, span, text, ul)
import Html.Attributes exposing (class, href, rel, src, style)
import Http
import Json.Decode as Json
import Loomio
import Markdown
import Url
import Url.Builder as UB



-- TODO
-- Make pretty
-- Fix avatars (gravatar / url)
-- Construct URL
-- Get element attributes
-- Link to comment field in loomio
-- Match @username and make <strong>


type Msg
    = GotComments (Result Http.Error (List Loomio.Comment))
    | GotDiscussion (Result Http.Error Loomio.DiscussionInfo)
    | NoOp


type alias State =
    { baseUrl : Url.Url
    , discussionKey : String
    , comments : List Loomio.Comment
    }


type alias Model =
    Maybe State



-- discussionUrl : String -> Url.Url
-- "https://talk.theborderland.se/api/v1/events?from=-10&per=20&order=sequence_id&discussion_id=965"


initialModelDecoder =
    Json.map3 State
        (Json.field "endpoint" Json.string
            |> Json.map Url.fromString
            |> Json.andThen
                (\x ->
                    case x of
                        Just u ->
                            Json.succeed u

                        Nothing ->
                            Json.fail "Invalid endpoint url"
                )
        )
        (Json.field "discussionKey" Json.string)
        (Json.succeed [])


init : Json.Value -> ( Model, Cmd Msg )
init flags =
    let
        model =
            flags
                |> Json.decodeValue initialModelDecoder
                |> Result.toMaybe
                |> Maybe.andThen
                    (\m ->
                        if String.length m.discussionKey > 0 then
                            Just m

                        else
                            Nothing
                    )
    in
    ( model
    , case model of
        Just m ->
            Http.get
                { url =
                    Loomio.apiUrl
                        m.baseUrl
                        Loomio.Discussions
                        (Just m.discussionKey)
                        []
                        |> Url.toString
                , expect =
                    Http.expectJson
                        GotDiscussion
                        (Json.field "discussions" <|
                            Json.index 0 <|
                                Loomio.decodeDiscussion
                        )
                }

        Nothing ->
            Cmd.none
    )


viewComments : State -> List Loomio.Comment -> Html msg
viewComments model cs =
    ul [ class "list-group" ] <| (cs |> List.reverse |> List.map (viewComment model))


viewComment : State -> Loomio.Comment -> Html msg
viewComment model c =
    li [ class "list-group-item" ]
        [ viewUser model c.user
        , Markdown.toHtml [] c.body
        ]


viewUser : State -> Loomio.User -> Html msg
viewUser model u =
    let
        wantedAvatar =
            Loomio.userAvatarUrl Loomio.Medium u |> Maybe.map Url.toString

        avatarSize =
            String.append (Loomio.avatarPixelSize Loomio.Medium |> String.fromInt) "px"

        avatarTag =
            case wantedAvatar of
                Just url ->
                    img
                        [ src url
                        , style "border-radius" "50% 50% 50% 50%"
                        , style "margin-right" "20px"
                        ]
                        []

                Nothing ->
                    div
                        [ style "width" avatarSize
                        , style "height" avatarSize
                        , style "margin-right" "20px"
                        , style "border" "1px solid gray"
                        , style "border-radius" "50%"
                        , style "display" "inline-block"
                        , style "vertical-align" "middle"
                        ]
                        []
    in
    div [ style "margin-bottom" "15px" ]
        [ avatarTag
        , span [ style "font-weight" "bold" ] [ text u.name ]
        ]


view : Model -> Html Msg
view model =
    case model of
        Just m ->
            Html.div []
                [ -- Html.node "link" [ rel "stylesheet", href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" ] [] -- for reactor
                viewComments m m.comments
                ]

        Nothing ->
            Html.div [] [ text "couldn't fetch comments" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotDiscussion result, Just m ) ->
            case result of
                Ok discussionInfo ->
                    ( model
                    , Http.get
                        { url =
                            Loomio.apiUrl
                                m.baseUrl
                                Loomio.Events
                                Nothing
                                [ UB.int "from" -10, UB.int "per" 20, UB.string "order" "sequence_id", UB.int "discussion_id" discussionInfo.id ]
                                |> Url.toString
                        , expect = Http.expectJson GotComments (Loomio.decodeComments m.baseUrl)
                        }
                    )

                Err s ->
                    ( model, Cmd.none )

        ( GotComments result, Just m ) ->
            ( Just { m | comments = Result.withDefault m.comments result }, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Json.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
