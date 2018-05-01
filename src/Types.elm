module Types exposing (..)

type alias Model = 
  { instanceUri : String
  , peers : List Peer
  }


type alias Peer = 
  (String, Bool)


type Msg = 
  Peer String
  | RemovePeer String
  | DisconnectPeer String
  | NowDisconnectPeer String
  | Signal String
  | ConnectPeer String
  | Error String
