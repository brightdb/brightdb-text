module Main exposing (..)

import Types
import View
import State
import Sub
import Html


init instanceUri =
    State.init instanceUri
        ! []


main : Program String Types.Model Types.Msg
main =
    Html.programWithFlags
        { view = View.view
        , init = init
        , update = State.update
        , subscriptions = Sub.subscriptions
        }
