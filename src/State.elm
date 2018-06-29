module State exposing (..)

import Types exposing (..)
import Encoder exposing (..)
import Bright
import Tuple exposing (..)
import Char
import String
import Array.Hamt as Array exposing (Array)
import Sequence exposing (Sequence, Path, Value(..), Entry(..), minPath, maxPath, mvrToList)
import Bitwise exposing (shiftLeftBy)
import Color exposing (Color, hsl)
import IntDict
import Dict


init : String -> Model
init instanceUri =
    { instanceUri = instanceUri
    , peers = []
    , text = Sequence.empty
    , history = Array.empty
    , cursor = ( minPath, maxPath )
    , colors =
        List.range 0 9
            |> List.map (toFloat >> (*) 25)
            |> List.map (\hue -> Color.hsl (degrees hue) 0.7 0.9)
    }


initPeer : String -> Color -> Peer
initPeer uri color =
    { uri = uri
    , connected = False
    , version = 0
    , color = color
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
            let
                ( color, colors ) =
                    case model.colors of
                        first :: rest ->
                            ( first, rest ++ [ first ] )

                        _ ->
                            ( Color.white, model.colors )
            in
                { model
                    | peers = initPeer peer color :: model.peers
                    , colors = colors
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

                paths =
                    List.map .path newOps

                minP =
                    List.foldl
                        (\path min ->
                            if Sequence.comparePath path min == LT then
                                path
                            else
                                min
                        )
                        maxPath
                        paths

                maxP =
                    List.foldl
                        (\path max ->
                            if Sequence.comparePath path max == GT then
                                path
                            else
                                max
                        )
                        minPath
                        paths

                cStart =
                    first model.cursor

                cStartNew =
                    if Sequence.comparePath cStart maxP == LT && Sequence.comparePath cStart minP == GT then
                        maxP
                    else
                        cStart

                cEndNew =
                    Sequence.after cStartNew text
                        |> Maybe.map first
                        |> Maybe.withDefault maxPath

                cmd =
                    if List.isEmpty newOps then
                        []
                    else
                        model.peers
                            |> List.filter (.connected >> (==) True)
                            |> List.map
                                (.uri >> encodeData (encodeOps newOps) >> Bright.outPort)
            in
                { model
                    | text = text
                    , cursor = ( cStartNew, cEndNew )
                    , history =
                        Array.fromList newOps
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
                    ! cmd

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
                        |> Maybe.withDefault minPath
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
                        |> Maybe.withDefault minPath
            in
                { model
                    | cursor = ( last, maxPath )
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

        KeyDown key ->
            case key of
                37 ->
                    let
                        next =
                            Sequence.before (first model.cursor) model.text
                                |> Maybe.map first
                                |> Maybe.withDefault minPath
                    in
                        { model
                            | cursor = ( next, first model.cursor )
                        }
                            ! []

                39 ->
                    let
                        next =
                            Sequence.after (second model.cursor) model.text
                                |> Maybe.map first
                                |> Maybe.withDefault maxPath
                    in
                        { model
                            | cursor = ( second model.cursor, next )
                        }
                            ! []

                8 ->
                    case Sequence.get (first model.cursor) model.text of
                        Nothing ->
                            model ! []

                        Just entry ->
                            delete ( first model.cursor, entry ) model

                _ ->
                    model ! []


delete : ( Path, Entry Char ) -> Model -> ( Model, Cmd Msg )
delete ( path, entry ) model =
    case getTarget entry of
        Nothing ->
            case Sequence.before path model.text of
                Nothing ->
                    model ! []

                Just next ->
                    delete next model

        Just target ->
            let
                op =
                    Sequence.createRemove model.instanceUri target path

                ( text, newOps ) =
                    Sequence.apply [ op ] model.text

                history =
                    Array.fromList newOps
                        |> Array.append model.history
            in
                { model
                    | text = text
                    , history = history
                }
                    ! (model.peers
                        |> List.filter (.connected >> (==) True)
                        |> List.map
                            (.uri >> encodeData (encodeOps [ op ]) >> Bright.outPort)
                      )


getTarget entry =
    case entry of
        Single origin (Value _) ->
            Just origin

        Concurrent mvr ->
            mvrToList mvr
                |> List.filter
                    (\( _, value ) ->
                        case value of
                            Value v ->
                                True

                            _ ->
                                False
                    )
                |> List.reverse
                |> List.head
                |> Maybe.map first

        _ ->
            Nothing
