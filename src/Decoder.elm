module Decoder exposing (..)

import Types exposing (..)
import Json.Decode exposing (..)

decodeMessage : Value -> Msg
decodeMessage value =
  case decodeValue message value of
    Err err ->
      Error err
    Ok msg ->
      msg


message : Decoder Msg
message =
  let
      body type_ =
        case Debug.log "type" type_ of
          "peer" ->
            field "uri" string
            |> map Peer
          _ ->
            "unknown type " ++ type_
            |> fail

  in
    field "type" string
    |> andThen body
