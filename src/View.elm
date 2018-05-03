module View exposing (view)

import Types exposing (..)
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Color
import Color.Convert exposing (colorToCssRgba)
import Style exposing (..)
import Style.Font as Font
import Style.Border as Border
import Style.Color as Color
import Element exposing (..)
import Element.Input as Input
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Sequence exposing (Sequence)
import Value exposing (Entry(..), Value(..))
import Dict
import Tuple exposing (..)
import Json.Decode as Dec


type Styles
    = None
    | Main
    | Navbar
    | Content
    | Title
    | Peer
    | Version


type Variations
    = Connected


navbarColor =
    Color.rgb 233 240 248


connectedColor =
    Color.rgb 233 248 234


textColor =
    Color.rgb 81 81 81


titleColor =
    Color.rgb 35 107 188


ubuntuFont =
    Font.importUrl
        { url = "https://fonts.googleapis.com/css?family=Ubuntu:400,700"
        , name = "Ubuntu"
        }


stylesheet =
    Style.styleSheet
        [ style None []
        , style Title
            [ Font.bold
            , Color.text titleColor
            ]
        , style Main
            [ Font.typeface [ ubuntuFont, Font.font "Arial", Font.font "Helvetica", Font.sansSerif ]
            ]
        , style Navbar
            [ Color.background navbarColor
            , Border.bottom 1
            , Border.solid
            , Color.border navbarColor
            ]
        , style Content
            [ Border.right 1
            , Border.solid
            , Color.border navbarColor
            ]
        , style Peer
            [ cursor "pointer"
            , variation Connected
                [ Font.bold
                ]
            ]
        , style Version
            [ Color.text Color.grey
            , Font.size 10
            ]
        ]


view : Model -> Html Msg
view model =
    Element.viewport stylesheet <|
        column Main
            [ height <| fill
            ]
            [ row
                Navbar
                [ width <| fill
                , padding 20
                ]
                [ el Title
                    []
                    (text model.instanceUri)
                ]
            , row None
                [ width <| fill
                , height <| fill
                ]
                [ content model, sidebar model ]
            ]


sidebar model =
    column None
        [ width <| percent 20
        ]
        (peers model.peers)


content model =
    Sequence.foldr (foldText model.cursor) [] model.text
        ++ cursorAtEnd model.cursor (Sequence.last model.text |> Maybe.map first)
        |> Html.div []
        |> Element.html
        |> (\x -> [ x ])
        |> column Content
            [ alignLeft
            , width <| fill
            , padding 5
            , scrollbars
            , id "container"
            , on "click"
                (Dec.at [ "target", "id" ] Dec.string
                    |> Dec.andThen
                        (\str ->
                            if str == "container" then
                                Dec.succeed ClickEnd
                            else
                                Dec.fail ""
                        )
                )
            ]


foldText cursor_ path entry result =
    entryToSpan cursor_ path entry
        ++ result


entryToSpan cursor path entry =
    case entry of
        Single origin (Value c) ->
            [ Html.span
                [ Html.onClick <| Click path
                , drawCursor cursor path
                    |> Html.style
                ]
                [ String.fromChar c
                    |> Html.text
                ]
            ]

        Concurrent mvr ->
            [ Html.span
                [ Dec.succeed (Click path)
                    |> Html.onWithOptions "click"
                        { stopPropagation = True
                        , preventDefault = False
                        }
                , drawCursor cursor path
                    |> Html.style
                ]
                (Dict.values mvr
                    |> List.filterMap
                        (\v ->
                            case v of
                                Value v ->
                                    Just v

                                _ ->
                                    Nothing
                        )
                    |> List.map (String.fromChar >> Html.text)
                )
            ]

        _ ->
            []


cursorStyle =
    ( "border-left", "solid 1px black" )


drawCursor ( start, end ) path =
    if end == path then
        [ cursorStyle ]
    else
        []


cursorAtEnd ( start, end ) lastPath =
    [ Html.span
        [ Html.style <|
            if lastPath == Nothing || Just start == lastPath then
                [ cursorStyle ]
            else
                []
        ]
        [ Html.text " "
        ]
    ]


peers =
    List.map
        (\peer ->
            let
                action =
                    if peer.connected then
                        DisconnectPeer
                    else
                        Signal
            in
                row Peer
                    [ onClick (action peer.uri)
                    , vary Connected peer.connected
                    , inlineStyle <|
                        if peer.connected then
                            [ ( "background-color", colorToCssRgba peer.color ) ]
                        else
                            []
                    , padding 5
                    ]
                    [ column None
                        [ width <| percent 90 ]
                        [ text peer.uri ]
                    , column Version
                        [ width <| percent 10 ]
                        [ text <| toString peer.version ]
                    ]
        )
