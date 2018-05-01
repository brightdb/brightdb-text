module State exposing (..)

import Types exposing (..)
import Encoder exposing (..)
import Bright

init : String -> Model
init instanceUri =
  { instanceUri = instanceUri
  , peers = []
  }


update : Msg -> Model -> (Model, Cmd Msg) 
update msg model =
  case msg of
    Connect peer ->
      model !
      [ encodePeer peer
        |> Bright.outPort
      ]
    Peer peer ->
      { model
        | peers = peer :: model.peers
      } ! []
    RemovePeer peer ->
      { model
        | peers = List.filter ((/=) peer) model.peers
      } ! []
    Error err ->
      model ! []
