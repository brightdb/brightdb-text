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
                        |> map PeerAvailable

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
                        (field "payload" decodePayload)

                _ ->
                    "unknown type "
                        ++ type_
                        |> fail
    in
        field "type" string
            |> andThen body


decodePayload : Decoder Payload
decodePayload =
    string
        |> andThen
            (\str ->
                case String.uncons str of
                    Just ( 'o', ops ) ->
                        case stringToOps ops of
                            Err err ->
                                fail err

                            Ok ops ->
                                Ops ops
                                    |> succeed

                    Just ( 's', sub ) ->
                        case stringToSubscription sub of
                            Err err ->
                                fail err

                            Ok subscription ->
                                Subscribe subscription
                                    |> succeed

                    _ ->
                        fail "unknown payload"
            )


stringToSubscription : String -> Result String Subscription
stringToSubscription str =
    case String.split "," str of
        version :: address :: [] ->
            case String.toInt version of
                Ok version ->
                    Subscription version address
                        |> Ok

                Err err ->
                    Err err

        _ ->
            Err <| "cannot decode subscription " ++ str


stringToOps : String -> Result String (List (Op Char))
stringToOps =
    String.split "|" >> List.map stringToOp >> resultListToResult


resultListToResult : List (Result String (Op Char)) -> Result String (List (Op Char))
resultListToResult result =
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
                Ok ops

            ( _, errs ) ->
                List.intersperse ", " errs
                    |> String.concat
                    |> Err


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
