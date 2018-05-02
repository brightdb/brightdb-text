module State exposing (..)

import Types exposing (..)
import Encoder exposing (..)
import Bright
import Tuple exposing (..)
import Char
import String
import Array.Hamt as Array exposing (Array)
import Sequence exposing (Sequence)
import Random.Pcg exposing (minInt, maxInt)


init : String -> Model
init instanceUri =
    { instanceUri = instanceUri
    , peers = []
    , text = Sequence.empty
    , history = Array.empty
    , cursor = ( minInt, maxInt )
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        Signal peer ->
            model
                ! [ encodeSignal peer
                        |> Bright.outPort
                  ]

        ConnectPeer peer ->
            { model
                | peers =
                    List.map
                        (\( p, c ) ->
                            if peer == p then
                                ( p, True )
                            else
                                ( p, c )
                        )
                        model.peers
            }
                ! []

        DisconnectPeer peer ->
            model
                ! [ encodeDisconnectPeer peer
                        |> Bright.outPort
                  ]

        NowDisconnectPeer peer ->
            { model
                | peers =
                    List.map
                        (\( p, c ) ->
                            if peer == p then
                                ( p, False )
                            else
                                ( p, c )
                        )
                        model.peers
            }
                ! []

        Peer peer ->
            { model
                | peers = ( peer, False ) :: model.peers
            }
                ! []

        RemovePeer peer ->
            { model
                | peers = List.filter (first >> (/=) peer) model.peers
            }
                ! []

        Error err ->
            model ! []

        Click path ->
            let
                start =
                    Sequence.before path model.text
                        |> Maybe.map first
                        |> Maybe.withDefault minInt
            in
                { model
                    | cursor = ( start, path )
                }
                    ! []

        ClickEnd ->
            let
                last =
                    Sequence.last model.text
                        |> Maybe.map first
                        |> Maybe.withDefault minInt
            in
                { model
                    | cursor = ( last, maxInt )
                }
                    ! []

        Key key ->
            let
                path =
                    Sequence.alloc (first model.cursor) (second model.cursor)

                op =
                    Char.fromCode key
                        |> Sequence.createInsert model.instanceUri path

                history =
                    Array.push op model.history

                text =
                    Sequence.apply [ op ] model.text

                cursor =
                    ( path, second model.cursor )
            in
                { model
                    | text = text
                    , history = history
                    , cursor = cursor
                }
                    ! []
