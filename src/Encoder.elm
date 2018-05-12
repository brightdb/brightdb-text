module Encoder exposing (..)

import Json.Encode exposing (..)
import Sequence exposing (Op, Operation(..), encodeOp)
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
    [ ( "s"
      , [ int version, string address ] |> list
      )
    ]
        |> object


encodeData : Value -> String -> Value
encodeData payload peer =
    [ ( "type", string "data" )
    , ( "peer", string peer )
    , ( "payload", encode 0 payload |> string )
    ]
        |> object


encodeOps : List (Op Char) -> Value
encodeOps ops =
    [ ( "o"
      , List.map (encodeOp (String.fromChar >> string)) ops
            |> list
      )
    ]
        |> object
