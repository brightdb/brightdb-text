module Decoder exposing (..)

import Types exposing (..)
import Json.Decode exposing (..)
import Sequence exposing (Op, Operation(..), decodeOp)
import String
import Result.Extra exposing (unpack)


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
                        |> map PeerAvailable

                "remove_peer" ->
                    field "uri" string
                        |> map RemovePeer

                "disconnect_peer" ->
                    field "uri" string
                        |> map NowDisconnectPeer

                "connect" ->
                    field "peer" string
                        |> map ConnectPeer

                "data" ->
                    map2 Data
                        (field "peer" string)
                        (field "payload" decodePayload)

                _ ->
                    "unknown type "
                        ++ type_
                        |> fail
    in
        field "type" string
            |> andThen body


decodePayload : Decoder Payload
decodePayload =
    string
        |> andThen
            (decodeString
                (oneOf
                    [ field "o" decodeOps
                        |> map Ops
                    , field "s" decodeSubscription
                        |> map Subscribe
                    ]
                )
                >> unpack fail succeed
            )


decodeSubscription : Decoder Subscription
decodeSubscription =
    map2 Subscription
        (index 0 int)
        (index 1 string)


decodeOps : Decoder (List (Op Char))
decodeOps =
    decodeOp decodeChar
        |> list


decodeChar : Decoder Char
decodeChar =
    string
        |> andThen
            (\str ->
                case String.uncons str of
                    Just ( char, "" ) ->
                        succeed char

                    _ ->
                        fail "could not decode char"
            )
