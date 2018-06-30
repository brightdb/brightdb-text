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
    , cursor = initCursor instanceUri
    , colors =
        List.range 0 9
            |> List.map (toFloat >> (*) 25)
            |> List.map (\hue -> Color.hsl (degrees hue) 0.7 0.9)
    , showTombs = False
    }


initCursor : String -> Cursor
initCursor origin =
    { left = minPath
    , right = maxPath
    , target = origin
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
                    model.cursor.left

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
                    , cursor =
                        updateCursor model.cursor cStartNew cEndNew model.cursor.target
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

        Click path target ->
            { model
                | cursor =
                    case Sequence.before path model.text of
                        Nothing ->
                            updateCursor model.cursor minPath path model.instanceUri

                        Just ( next, Single origin _ ) ->
                            updateCursor model.cursor next path origin

                        Just ( next, Concurrent mvr ) ->
                            let
                                list =
                                    mvrToList mvr |> filterTombs model.showTombs
                            in
                                case Dict.fromList list |> Dict.get target of
                                    Nothing ->
                                        case List.head list of
                                            Nothing ->
                                                updateCursor model.cursor next path model.instanceUri

                                            Just ( head, _ ) ->
                                                updateCursor model.cursor next path head

                                    Just _ ->
                                        updateCursor model.cursor next path target
            }
                ! []

        ClickEnd ->
            { model
                | cursor =
                    case Sequence.last model.text of
                        Nothing ->
                            updateCursor model.cursor minPath maxPath model.instanceUri

                        Just ( next, Single origin (Value _) ) ->
                            updateCursor model.cursor next maxPath origin

                        Just ( next, Single origin (Tomb _) ) ->
                            if model.showTombs then
                                updateCursor model.cursor next maxPath origin
                            else
                                updateCursor model.cursor next maxPath origin
                                    |> moveCursorLeft model.instanceUri model.showTombs model.text

                        Just ( next, Concurrent mvr ) ->
                            let
                                list =
                                    mvrToList mvr |> filterTombs model.showTombs
                            in
                                case Dict.fromList list |> Dict.get model.instanceUri of
                                    Just _ ->
                                        updateCursor model.cursor next maxPath model.instanceUri

                                    Nothing ->
                                        case List.head list of
                                            Nothing ->
                                                updateCursor model.cursor next maxPath model.instanceUri
                                                    |> moveCursorLeft model.instanceUri model.showTombs model.text

                                            Just ( head, _ ) ->
                                                updateCursor model.cursor next maxPath head
            }
                ! []

        Key key ->
            let
                path =
                    Sequence.alloc model.cursor.left model.cursor.right

                op =
                    Char.fromCode key
                        |> Sequence.createInsert model.instanceUri path

                ( text, newOps ) =
                    Sequence.apply [ op ] model.text

                history =
                    Array.fromList newOps
                        |> Array.append model.history

                cursor =
                    updateCursor model.cursor path model.cursor.right model.cursor.target
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
                    { model
                        | cursor = moveCursorLeft model.instanceUri model.showTombs model.text model.cursor
                    }
                        ! []

                39 ->
                    { model
                        | cursor = moveCursorRight model.instanceUri model.showTombs model.text model.cursor
                    }
                        ! []

                38 ->
                    case Sequence.get model.cursor.left model.text of
                        Nothing ->
                            model ! []

                        Just (Single _ _) ->
                            model ! []

                        Just (Concurrent mvr) ->
                            mvrToList mvr
                                |> filterTombs model.showTombs
                                |> List.foldl
                                    (\( current, _ ) result ->
                                        { result
                                            | previous = Just current
                                            , found =
                                                if current == model.cursor.target then
                                                    case result.previous of
                                                        Nothing ->
                                                            current

                                                        Just previous ->
                                                            previous
                                                else
                                                    result.found
                                        }
                                    )
                                    { found = model.cursor.target, previous = Nothing }
                                |> .found
                                |> updateCursor model.cursor model.cursor.left model.cursor.right
                                |> \cursor -> { model | cursor = cursor } ! []

                40 ->
                    case Sequence.get model.cursor.left model.text of
                        Nothing ->
                            model ! []

                        Just (Single _ _) ->
                            model ! []

                        Just (Concurrent mvr) ->
                            mvrToList mvr
                                |> filterTombs model.showTombs
                                |> List.foldl
                                    (\( current, _ ) result ->
                                        { result
                                            | previous = Just current
                                            , found =
                                                case result.previous of
                                                    Just previous ->
                                                        if previous == model.cursor.target then
                                                            current
                                                        else
                                                            previous

                                                    Nothing ->
                                                        result.found
                                        }
                                    )
                                    { found = model.cursor.target, previous = Nothing }
                                |> .found
                                |> updateCursor model.cursor model.cursor.left model.cursor.right
                                |> \cursor -> { model | cursor = cursor } ! []

                8 ->
                    case Sequence.get model.cursor.left model.text of
                        Nothing ->
                            model ! []

                        Just entry ->
                            delete model.cursor.left model.cursor.target entry model

                _ ->
                    model ! []

        ToggleTombs ->
            { model
                | showTombs = not model.showTombs
            }
                ! []


filterTombs : Bool -> List ( String, Value a ) -> List ( String, Value a )
filterTombs showTombs =
    List.filter
        (\( _, value ) ->
            case value of
                Value v ->
                    True

                Tomb _ ->
                    showTombs
        )


moveCursorLeft : String -> Bool -> Sequence x -> Cursor -> Cursor
moveCursorLeft instanceUri showTombs text cursor =
    if cursor.left == minPath then
        cursor
    else
        case Sequence.before cursor.left text of
            Nothing ->
                updateCursor cursor minPath cursor.left cursor.target

            Just ( next, Single origin (Value _) ) ->
                updateCursor cursor next cursor.left origin

            Just ( next, Single origin (Tomb _) ) ->
                if showTombs then
                    updateCursor cursor next cursor.left origin
                else
                    updateCursor cursor next cursor.left origin
                        |> moveCursorLeft instanceUri showTombs text

            Just ( next, Concurrent mvr ) ->
                let
                    list =
                        mvrToList mvr |> filterTombs showTombs
                in
                    case Dict.fromList list |> Dict.get cursor.target of
                        Just _ ->
                            updateCursor cursor next cursor.left cursor.target

                        Nothing ->
                            case List.head list of
                                Nothing ->
                                    updateCursor cursor next cursor.left instanceUri
                                        |> moveCursorLeft instanceUri showTombs text

                                Just ( head, _ ) ->
                                    updateCursor cursor next cursor.left head


updateCursor : Cursor -> Path -> Path -> String -> Cursor
updateCursor cursor left right target =
    { cursor
        | left = left
        , right = right
        , target = target
    }


findNextOriginBefore : String -> Bool -> Path -> Sequence x -> String
findNextOriginBefore target showTombs path text =
    case Sequence.before path text of
        Nothing ->
            target

        Just ( next, Single origin (Value _) ) ->
            origin

        Just ( next, Single origin (Tomb _) ) ->
            if showTombs then
                origin
            else
                findNextOriginBefore target showTombs next text

        Just ( next, Concurrent mvr ) ->
            let
                list =
                    mvrToList mvr
                        |> filterTombs showTombs
            in
                case Dict.fromList list |> Dict.get target of
                    Nothing ->
                        case List.head list of
                            Nothing ->
                                findNextOriginBefore target showTombs next text

                            Just head ->
                                first head

                    Just _ ->
                        target


moveCursorRight : String -> Bool -> Sequence x -> Cursor -> Cursor
moveCursorRight instanceUri showTombs text cursor =
    if cursor.right == maxPath then
        cursor
    else
        case Sequence.after cursor.right text of
            Nothing ->
                findNextOriginBefore cursor.target showTombs maxPath text
                    |> updateCursor cursor cursor.right maxPath

            Just ( next, Single origin (Value _) ) ->
                findNextOriginBefore cursor.target showTombs next text
                    |> updateCursor cursor cursor.right next

            Just ( next, Single origin (Tomb _) ) ->
                if showTombs then
                    findNextOriginBefore cursor.target showTombs next text
                        |> updateCursor cursor cursor.right next
                else
                    updateCursor cursor cursor.right next origin
                        |> moveCursorRight instanceUri showTombs text
                        |> (\cursor ->
                                findNextOriginBefore cursor.target showTombs cursor.right text
                                    |> updateCursor cursor cursor.left cursor.right
                           )

            Just ( next, Concurrent mvr ) ->
                let
                    list =
                        mvrToList mvr |> filterTombs showTombs
                in
                    if List.isEmpty list then
                        updateCursor cursor cursor.right next cursor.target
                            |> moveCursorRight instanceUri showTombs text
                            |> (\cursor ->
                                    findNextOriginBefore cursor.target showTombs cursor.right text
                                        |> updateCursor cursor cursor.left cursor.right
                               )
                    else
                        findNextOriginBefore cursor.target showTombs next text
                            |> updateCursor cursor cursor.right next


delete : Path -> String -> Entry Char -> Model -> ( Model, Cmd Msg )
delete path target entry model =
    case entry of
        Single target (Tomb _) ->
            case Sequence.before path model.text of
                Nothing ->
                    model ! []

                Just ( nextPath, nextEntry ) ->
                    if model.showTombs then
                        { model
                            | cursor = updateCursor model.cursor nextPath path model.cursor.target
                        }
                            ! []
                    else
                        delete nextPath target nextEntry model

        Single target (Value _) ->
            deleteThis model target path

        Concurrent mvr ->
            deleteThis model target path


deleteThis : Model -> String -> Path -> ( Model, Cmd Msg )
deleteThis model target path =
    let
        op =
            Sequence.createRemove model.instanceUri target path

        ( text, newOps ) =
            Sequence.apply [ op ] model.text

        history =
            Array.fromList newOps
                |> Array.append model.history

        next =
            Sequence.before path model.text
                |> Maybe.map first
                |> Maybe.withDefault minPath
    in
        { model
            | text = text
            , history = history
            , cursor = updateCursor model.cursor next path target
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
