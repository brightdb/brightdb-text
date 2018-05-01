module State exposing (..)

import Types exposing (..)
import Encoder exposing (..)
import Bright
import Tuple exposing (..)

init : String -> Model
init instanceUri =
  { instanceUri = instanceUri
  , peers = []
  }


update : Msg -> Model -> (Model, Cmd Msg) 
update msg model =
  case Debug.log "msg" msg of
    Signal peer ->
      model !
      [ encodeSignal peer
        |> Bright.outPort
      ]
    ConnectPeer peer ->
      { model
        | peers = List.map (\(p,c) -> if peer == p then (p, True) else (p, c)) model.peers
      } ! []
    DisconnectPeer peer ->
      model !
      [ encodeDisconnectPeer peer
        |> Bright.outPort
      ]
    NowDisconnectPeer peer ->
      { model
        | peers = List.map (\(p,c) -> if peer == p then (p, False) else (p, c)) model.peers
      } ! []
    Peer peer ->
      { model
        | peers = (peer, False) :: model.peers
      } ! []
    RemovePeer peer ->
      { model
        | peers = List.filter (first >> (/=) peer) model.peers
      } ! []
    Error err ->
      model ! []
