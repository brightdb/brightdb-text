module Encoder exposing (..)

import Json.Encode exposing (..)
import Sequence exposing (Op)
import Value exposing (Operation(..))
import String


encodeSignal : String -> Value
encodeSignal peer =
    [ ( "type", string "signal" )
    , ( "peer", string peer )
    ]
        |> object


encodeDisconnectPeer : String -> Value
encodeDisconnectPeer peer =
    [ ( "type", string "disconnect_peer" )
    , ( "uri", string peer )
    ]
        |> object


encodeData : List (Op Char) -> String -> Value
encodeData ops peer =
    [ ( "type", string "data" )
    , ( "peer", string peer )
    , ( "payload", encodeOps ops )
    ]
        |> object


encodeOps : List (Op Char) -> Value
encodeOps ops =
    List.map opToString ops
        |> List.intersperse "|"
        |> String.concat
        |> string


opToString : Op Char -> String
opToString op =
    [ op.origin
    , op.target
    , toString op.path
    , operationToString op.op
    ]
        |> List.intersperse ","
        |> String.concat


operationToString : Operation Char -> String
operationToString operation =
    case operation of
        Insert c ->
            "i" ++ String.fromChar c

        Remove ->
            "r"
