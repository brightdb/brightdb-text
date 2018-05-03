module Sub exposing (subscriptions)

import Bright
import Decoder exposing (decodeMessage)
import Keyboard
import Types exposing (..)


subscriptions model =
    [ Bright.inPort decodeMessage
    , Keyboard.presses Key
    , Keyboard.downs KeyUp
    ]
        |> Sub.batch
