module Loomio exposing (..)

import Json.Decode as Json exposing (field, at, string, int, list, map, map2, map3, map4, andThen, fail, succeed)
import Html exposing (Html, text, li, ul)
import Html.Attributes exposing (class)
import List exposing (head, filter)
import Dict exposing (Dict)
import Tuple

type alias User =
    { name : String
    , username : String
    , avatarUrls : Maybe String
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

decodeDiscussion : Json.Decoder DiscussionInfo
decodeDiscussion =
    map2 DiscussionInfo
        (field "id" int)
        (field "items_count" int)


decodeComments : Json.Decoder (List Comment)
decodeComments =
    (field "users" decodeUsers)
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

decodeUsers : Json.Decoder UserDict
decodeUsers =
    map2
    Tuple.pair
        (field "id" int) decodeUser
        |> list |> map Dict.fromList

decodeUser : Json.Decoder User
decodeUser =
    map3 User
        (field "name" string)
        (field "username" string)
        (succeed Nothing)
        --(at ["avatar_url", "large"] string)

viewComments : List Comment -> Html msg
viewComments cs =
    ul [ class "list-group" ] <| List.map viewComment cs

viewComment : Comment -> Html msg
viewComment c =
    li [ class "list-group-item" ] [ viewUser c.user ]

viewUser : User -> Html msg
viewUser u = text ""

