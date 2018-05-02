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


encodeSubscription : Int -> String -> Value
encodeSubscription version address =
    "s" ++ toString version ++ "," ++ address |> string


encodeData : Value -> String -> Value
encodeData payload peer =
    [ ( "type", string "data" )
    , ( "peer", string peer )
    , ( "payload", payload )
    ]
        |> object


encodeOps : List (Op Char) -> Value
encodeOps ops =
    List.map opToString ops
        |> List.intersperse "|"
        |> String.concat
        |> (++) "o"
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
