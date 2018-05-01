port module Bright exposing (..)

import Json.Encode as Enc
import Json.Decode as Dec


port outPort : Enc.Value -> Cmd msg


port inPort : (Dec.Value -> msg) -> Sub msg
