module Loomio exposing (..)

import Json.Decode as Json exposing (field)

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

type alias Discussion =
    { id : Int
    , comments : List Comment
    }

type alias Response =
    {
    }

decodeResponse : Json.Decoder Response
decodeResponse =
    field
