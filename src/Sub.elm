module Sub exposing (subscriptions)

import Bright 
import Decoder exposing (decodeMessage)

subscriptions model =
  Bright.inPort decodeMessage
