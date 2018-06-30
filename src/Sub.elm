module Sub exposing (subscriptions)

import Bright
import Decoder exposing (decodeMessage)
import Keyboard
import Types exposing (..)
import Time


subscriptions model =
    [ Bright.inPort decodeMessage
    , Keyboard.presses Key
    , Keyboard.downs KeyDown
    , Time.every 500 (always Blink)
    ]
        |> Sub.batch
