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


initPeer : String -> Peer
initPeer uri =
    { uri = uri
    , connected = False
    , version = 0
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
            let
                ( peers, v ) =
                    List.foldr
                        (\peer_ ( peers, v ) ->
                            case v of
                                Just v ->
                                    ( peer_ :: peers, Just v )

                                Nothing ->
                                    if peer_.uri == peer then
                                        ( { peer_
                                            | connected = True
                                          }
                                            :: peers
                                        , Just peer_.version
                                        )
                                    else
                                        ( peer_ :: peers, v )
                        )
                        ( [], Nothing )
                        model.peers

                cmd =
                    case v of
                        Nothing ->
                            []

                        Just v ->
                            [ encodeData (encodeSubscription v "*") peer
                                |> Bright.outPort
                            ]
            in
                { model
                    | peers = peers
                }
                    ! cmd

        DisconnectPeer peer ->
            model
                ! [ encodeDisconnectPeer peer
                        |> Bright.outPort
                  ]

        NowDisconnectPeer peer ->
            { model
                | peers =
                    List.map
                        (\p ->
                            if peer == p.uri then
                                { p
                                    | connected = False
                                }
                            else
                                p
                        )
                        model.peers
            }
                ! []

        PeerAvailable peer ->
            { model
                | peers = initPeer peer :: model.peers
            }
                ! []

        RemovePeer peer ->
            { model
                | peers = List.filter (.uri >> (/=) peer) model.peers
            }
                ! []

        Error err ->
            model ! []

        Data peer (Ops ops) ->
            let
                ( text, newOps ) =
                    Sequence.apply ops model.text
            in
                { model
                    | text = text
                    , history =
                        Array.fromList newOps
                            |> Debug.log "newOps"
                            |> Array.append model.history
                    , peers =
                        List.map
                            (\peer_ ->
                                if peer_.uri == peer then
                                    { peer_
                                        | version = peer_.version + List.length newOps
                                    }
                                else
                                    peer_
                            )
                            model.peers
                }
                    ! []

        Data peer (Subscribe subscription) ->
            let
                changes =
                    Array.slice subscription.version (Array.length model.history) model.history
                        |> Array.toList

                cmd =
                    if List.isEmpty changes then
                        []
                    else
                        [ encodeData (encodeOps changes) peer
                            |> Bright.outPort
                        ]
            in
                model
                    ! cmd

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

                ( text, newOps ) =
                    Sequence.apply [ op ] model.text

                history =
                    Array.fromList newOps
                        |> Array.append model.history

                cursor =
                    ( path, second model.cursor )
            in
                { model
                    | text = text
                    , history = history
                    , cursor = cursor
                }
                    ! (model.peers
                        |> List.filter (.connected >> (==) True)
                        |> List.map
                            (.uri >> encodeData (encodeOps [ op ]) >> Bright.outPort)
                      )
