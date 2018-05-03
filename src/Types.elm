module Types exposing (..)

import Sequence exposing (Sequence, Op, Path)
import Array.Hamt as Array exposing (Array)
import Json.Decode as Dec
import Color exposing (Color)


type alias Model =
    { instanceUri : String
    , peers : List Peer
    , text : Sequence Char
    , history : Array (Op Char)
    , cursor : ( Path, Path )
    , colors : List Color
    }


type alias Peer =
    { uri : String
    , connected : Bool
    , version : Int
    , color : Color
    }


type alias Subscription =
    { version : Int
    , address : String
    }


type Payload
    = Ops (List (Op Char))
    | Subscribe Subscription


type Msg
    = PeerAvailable String
    | RemovePeer String
    | DisconnectPeer String
    | NowDisconnectPeer String
    | Signal String
    | ConnectPeer String
    | Data String Payload
    | Error String
    | Click Path
    | ClickEnd
    | Key Int
    | KeyUp Int
