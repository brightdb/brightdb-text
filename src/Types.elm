module Types exposing (..)

import Sequence exposing (Sequence, Value(..), Op, Path)
import Array.Hamt as Array exposing (Array)
import Json.Decode as Dec
import Color exposing (Color)
import Time exposing (Time)


type alias Model =
    { instanceUri : String
    , peers : List Peer
    , text : Sequence Char
    , history : Array (Op Char)
    , cursor : Cursor
    , colors : List Color
    , showTombs : Bool
    , blink : Bool
    }


type alias Cursor =
    { left : Path
    , right : Path
    , target : String -- delete target when in an MVR
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
    | Click Path String
    | ClickEnd
    | Key Int
    | KeyDown Int
    | ToggleTombs
    | Blink


isValue : Value a -> Bool
isValue v =
    case v of
        Value _ ->
            True

        Tomb _ ->
            False
