module Types exposing (..)

type alias Model = 
  { instanceUri : String
  , peers : List Peer
  }


type alias Peer = 
  String


type Msg = 
  Peer String
  | RemovePeer String
  | Connect String
  | Error String
