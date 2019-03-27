module Loomio exposing (..)

import Json.Decode as Json exposing (field, at, string, int, list, map, map2, map3, map4, andThen, fail, succeed)
import Html exposing (Html, text, li, ul, img, div, span)
import Html.Attributes exposing (class, src, style)
import List exposing (head, filter)
import Dict exposing (Dict)
import Tuple
import Url
import Url.Builder

import Markdown

type alias User =
    { name : String
    , username : String
    , avatarUrls : Maybe (Url.Url, Url.Url, Url.Url)
    }

type alias Comment =
    { body : String
    , createdAt : String
    , updatedAt : String
    , user : User
    }

type alias DiscussionInfo =
    { id : Int
    , numComments : Int
    }

type alias UserDict = Dict Int User

type AvatarSize
    = Small
    | Medium
    | Large

userAvatar : AvatarSize -> User -> Maybe Url.Url
userAvatar sz user =
    user.avatarUrls
    |> Maybe.map (\(s, m, l) ->
        case sz of
            Small ->
                s
            Medium ->
                m
            Large ->
                l
    )

type Collection
    = Discussions
    | Events

apiUrl : Url.Url -> Collection -> Maybe String -> (List Url.Builder.QueryParameter) -> Url.Url
apiUrl base collection id params =
    let
        collectionName =
            case collection of
                Discussions ->
                    "discussions"
                Events ->
                    "events"
        collectionId =
            id
            |> Maybe.map (String.append "/")
            |> Maybe.withDefault ""
    in
    
    { base
        | path = "/api/v1/" ++ collectionName ++ collectionId
        , query = Just <| String.dropLeft 1 <| Url.Builder.toQuery params }

decodeDiscussion : Json.Decoder DiscussionInfo
decodeDiscussion =
    map2 DiscussionInfo
        (field "id" int)
        (field "items_count" int)

decodeComments : Url.Url -> Json.Decoder (List Comment)
decodeComments baseUrl =
    (field "users" (decodeUsers baseUrl))
    |> andThen (\u -> field "comments" <| list (decodeComment u))

decodeComment : UserDict -> Json.Decoder Comment
decodeComment users =
    map4 Comment
        (field "body" string)
        (field "created_at" string)
        (field "updated_at" string)
        (field "author_id" int
            |> andThen (\id ->
                            case Dict.get id users of
                                Nothing ->
                                    fail "Unknown user"
                                Just u ->
                                    succeed u
                       ))

decodeUploadedAvatarUrls : Url.Url -> Json.Decoder (Maybe (Url.Url, Url.Url, Url.Url))
decodeUploadedAvatarUrls baseUrl =
    let
        triple a b c = (a, b, c)
        urlPathString = map (\p -> Just { baseUrl | path = p }) string
    in  
        map3 triple
            (field "small" urlPathString)
            (field "medium" urlPathString)
            (field "large" urlPathString)
        |> map (\(s, m, l) ->
            Maybe.map3 triple s m l
        )

gravatarUrls : String -> (Url.Url, Url.Url, Url.Url)
gravatarUrls emailHash =
    let
        size s =
            case s of
                Small ->
                    30
                Medium ->
                    50
                Large ->
                    170

        url s =
            { protocol = Url.Https
            , host = "www.gravatar.com"
            , port_ = Nothing
            , path = "/avatar/" ++ emailHash
            , query = Just <| String.dropLeft 1 <| Url.Builder.toQuery [ Url.Builder.int "s" (size s) ]
            , fragment = Nothing
            }
    in
        (url Small, url Medium, url Large)


decodeUsers : Url.Url -> Json.Decoder UserDict
decodeUsers baseUrl =
    map2
    Tuple.pair
        (field "id" int) (decodeUser baseUrl)
        |> list |> map Dict.fromList

decodeUser : Url.Url -> Json.Decoder User
decodeUser baseUrl =
    map3 User
        (field "name" string)
        (field "username" string)
        (field "avatar_kind" string
            |> andThen (\kind ->
                case kind of
                    "uploaded" ->
                        field "avatar_url" (decodeUploadedAvatarUrls baseUrl)
                    "gravatar" ->
                        field "email_hash" (string |> map gravatarUrls |> map Just)
                    _ ->
                        succeed Nothing)
        )

viewComments : List Comment -> Html msg
viewComments cs =
    ul [ class "list-group" ] <| List.map viewComment (List.reverse cs)

viewComment : Comment -> Html msg
viewComment c =
    li [ class "list-group-item" ]
        [ viewUser c.user
        , Markdown.toHtml [] c.body
        ]

viewUser : User -> Html msg
viewUser u = div [ style "margin-bottom" "15px" ]
             [ img
                [ src
                    (userAvatar Medium u
                        |> Maybe.map Url.toString
                        |> Maybe.withDefault "https://partycity6.scene7.com/is/image/PartyCity/_pdp_sq_?$_1000x1000_$&$product=PartyCity/176114"
                    )
                , (style "border-radius" "50% 50% 50% 50%")
                , (style "margin-right" "20px")
                ]
                []
             , span [ style "font-weight" "bold" ] [ text u.name ]
             ]

