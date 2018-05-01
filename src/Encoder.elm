module Encoder exposing (..)

import Json.Encode exposing (..)

encodePeer : String -> Value
encodePeer peer =
  string peer
