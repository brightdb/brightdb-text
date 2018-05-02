module Decoder exposing (..)

import Types exposing (..)
import Json.Decode exposing (..)
import Sequence exposing (Op)
import Value exposing (Operation(..))
import String


decodeMessage : Value -> Msg
decodeMessage value =
    case decodeValue message value of
        Err err ->
            Error err

        Ok msg ->
            msg


message : Decoder Msg
message =
    let
        body type_ =
            case Debug.log "type" type_ of
                "peer" ->
                    field "uri" string
                        |> map Peer

                "remove_peer" ->
                    field "uri" string
                        |> map RemovePeer

                "disconnect_peer" ->
                    field "uri" string
                        |> map NowDisconnectPeer

                "connect" ->
                    field "peer" string
                        |> map ConnectPeer

                "data" ->
                    map2 Data
                        (field "peer" string)
                        (field "payload" decodeOps)

                _ ->
                    "unknown type "
                        ++ type_
                        |> fail
    in
        field "type" string
            |> andThen body


decodeOps : Decoder (List (Op Char))
decodeOps =
    string
        |> andThen (String.split "|" >> List.map stringToOp >> resultToDecoder)


resultToDecoder : List (Result String (Op Char)) -> Decoder (List (Op Char))
resultToDecoder result =
    let
        ( ops, errs ) =
            result
                |> List.foldr
                    (\result ( oks, errs ) ->
                        case result of
                            Ok op ->
                                ( op :: oks, errs )

                            Err err ->
                                ( oks, err :: errs )
                    )
                    ( [], [] )
    in
        case ( ops, errs ) of
            ( ops, [] ) ->
                succeed ops

            ( _, errs ) ->
                List.intersperse ", " errs
                    |> String.concat
                    |> fail


stringToOp : String -> Result String (Op Char)
stringToOp str =
    case String.split "," str of
        origin :: target :: path :: operation :: [] ->
            case ( String.toInt path, stringToOperation operation ) of
                ( Ok path, Ok operation ) ->
                    Op origin target path operation
                        |> Ok

                _ ->
                    "could not decode " ++ str |> Err

        _ ->
            "could not decode " ++ str |> Err


stringToOperation : String -> Result String (Operation Char)
stringToOperation str =
    case String.toList str of
        'i' :: char :: [] ->
            Insert char
                |> Ok

        'r' :: [] ->
            Ok Remove

        _ ->
            Err <| "unknown operation " ++ str
