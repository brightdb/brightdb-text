module Types exposing (..)

import Sequence exposing (Sequence, Op, Path)
import Array.Hamt as Array exposing (Array)
import Json.Decode as Dec


type alias Model =
    { instanceUri : String
    , peers : List Peer
    , text : Sequence Char
    , history : Array (Op Char)
    , cursor : ( Path, Path )
    }


type alias Peer =
    ( String, Bool )


type Msg
    = Peer String
    | RemovePeer String
    | DisconnectPeer String
    | NowDisconnectPeer String
    | Signal String
    | ConnectPeer String
    | Error String
    | Click Path
    | ClickEnd
    | Key Int
