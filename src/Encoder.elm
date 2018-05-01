module Encoder exposing (..)

import Json.Encode exposing (..)

encodeSignal : String -> Value
encodeSignal peer =
  [ ("type", string "signal") 
  , ("peer", string peer)
  ] |> object


encodeDisconnectPeer : String -> Value
encodeDisconnectPeer peer =
  [ ("type", string "disconnect_peer") 
  , ("uri", string peer)
  ] |> object
