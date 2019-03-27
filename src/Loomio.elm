module Loomio exposing (..)

import Json.Decode as Json exposing (field, at, string, int, list, map2, map3, map4, andThen, fail, succeed)
import Html exposing (Html, text)
import List exposing (head, filter)
import Dict exposing (Dict)
type alias User =
    { name : String
    , username : String
    , avatarUrls : String
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

decodeUser : Json.Decoder User
decodeUser =
    map3 User
        (field "name" string)
        (field "username" string)
        (at ["avatar_url", "large"] string)

viewComment : Comment -> Html msg
viewComment _ =
    text ""


