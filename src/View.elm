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
import Sequence exposing (Sequence, Value(..), TombValue(..), Entry(..), mvrToList)
import Dict
import Tuple exposing (..)
import Json.Decode as Dec
import List.Extra
import String
import Css


type Class
    = ContentFrame
    | SingleChar
    | ConcurrentChar
    | Space
    | Deleted


rules =
    let
        charHeight =
            ( "height", "1em" )
    in
        [ { selectors = [ Css.Class ContentFrame ]
          , descriptor = [ ( "display", "flex" ), ( "align-items", "center" ) ]
          }
        , { selectors = [ Css.Class SingleChar ]
          , descriptor = [ ( "display", "flex" ), charHeight ]
          }
        , { selectors = [ Css.Class Space ]
          , descriptor = [ ( "width", "3pt" ) ]
          }
        , { selectors = [ Css.Class ConcurrentChar ]
          , descriptor = [ ( "display", "flex" ), ( "flex-direction", "column" ) ]
          }
        , { selectors = [ Css.Descendant (Css.Type "span") (Css.Class ConcurrentChar) ]
          , descriptor = [ charHeight ]
          }
        , { selectors = [ Css.Class Deleted ]
          , descriptor = [ ( "text-decoration", "line-through" ), ( "color", "grey" ) ]
          }
        ]


stylesheetCss =
    Css.stylesheet [] rules


type Styles
    = None
    | Main
    | Navbar
    | Content
    | Config
    | Title
    | Name
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
        , style Name
            [ Color.text titleColor
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
        , style Config
            [ Border.top 1
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


prettyInstanceUri name =
    String.split "/" name
        |> List.drop 1
        |> List.head
        |> Maybe.withDefault name


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
                [ column None
                    [ width <| percent 50
                    ]
                    [ el Title
                        []
                        (text "Very Simple Collaborative Text Editor")
                    ]
                , column None
                    [ width <| percent 50
                    , alignRight
                    ]
                    [ el Name
                        []
                        (model.instanceUri
                            |> prettyInstanceUri
                            |> (\uri -> uri ++ " (You)")
                            |> text
                        )
                    ]
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
        , minWidth <| px 200
        , scrollbars
        ]
        [ row None
            [ height <| fill
            , width <| fill
            ]
            [ peersSidebar model
            ]
        , row Config
            [ height <| px 30
            , width <| fill
            ]
            [ el None
                [ padding 5
                ]
                (Input.checkbox None
                    []
                    { onChange = always ToggleTombs
                    , checked = model.showTombs
                    , label = el None [] (text "show deleted chars")
                    , options = []
                    }
                )
            ]
        ]


peersSidebar model =
    column None
        [ scrollbars
        ]
        (peers model.peers)


content model =
    Sequence.foldr (foldText model) [] model.text
        ++ cursorAtEnd model.cursor (Sequence.last model.text |> Maybe.map first)
        |> Html.div [ stylesheetCss.class ContentFrame ]
        |> (\c -> Html.div [] [ Css.style [] stylesheetCss, c ])
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


foldText model path entry result =
    entryToSpan model path entry
        ++ result


unpackValue showTombs value =
    case value of
        Value v ->
            Just ( v, True )

        Tomb (TombValue v) ->
            if showTombs then
                Just ( v, False )
            else
                Nothing

        Tomb TombUnknown ->
            Nothing


printValue model path value origin =
    case unpackValue model.showTombs value of
        Nothing ->
            []

        Just ( v, isValue ) ->
            [ Html.div
                [ Html.onClick <| Click path origin
                , SingleChar
                    :: (if v == ' ' then
                            [ Space ]
                        else
                            []
                       )
                    ++ (if isValue then
                            []
                        else
                            [ Deleted ]
                       )
                    |> stylesheetCss.classes
                , peerTextStyle model.instanceUri model.peers origin
                , drawCursor model.cursor path origin
                    |> Html.style
                ]
                [ String.fromChar v
                    |> Html.text
                ]
            ]


entryToSpan model path entry =
    case entry of
        Single origin value ->
            printValue model path value origin

        Concurrent mvr ->
            mvrToList mvr
                |> List.filterMap
                    (\( target, v ) ->
                        unpackValue model.showTombs v
                            |> Maybe.map ((,) target)
                    )
                |> List.map
                    (\( target, ( v, isValue ) ) ->
                        String.fromChar v
                            |> Html.text
                            |> \t ->
                                Html.span
                                    [ drawCursor model.cursor path target
                                        |> Html.style
                                    , Dec.succeed (Click path target)
                                        |> Html.onWithOptions "click"
                                            { stopPropagation = True
                                            , preventDefault = False
                                            }
                                    , peerTextStyle model.instanceUri model.peers target
                                    , if isValue then
                                        Html.attribute "x" ""
                                      else
                                        stylesheetCss.class Deleted
                                    ]
                                    [ t ]
                    )
                |> Html.div
                    [ stylesheetCss.class ConcurrentChar
                    ]
                |> \x -> [ x ]


cursorStyle =
    "solid 1px black"


cursorStyleLeft =
    ( "border-left", cursorStyle )


cursorStyleRight =
    ( "border-right", cursorStyle )


peerToColor instanceUri peers origin =
    if origin == instanceUri then
        colorToCssRgba navbarColor
    else
        List.Extra.find (.uri >> (==) origin) peers
            |> Maybe.map (.color >> colorToCssRgba)
            |> Maybe.withDefault "#fff"


peerTextStyle instanceUri peers origin =
    Html.style
        [ ( "background-color", peerToColor instanceUri peers origin ) ]


peerTextStyleGradient instanceUri peers entry =
    Html.style
        [ ( "background"
          , case entry of
                Single origin _ ->
                    peerToColor instanceUri peers origin

                Concurrent mvr ->
                    let
                        colors =
                            mvrToList mvr
                                |> List.map (first >> peerToColor instanceUri peers)

                        len =
                            List.length colors
                                |> Debug.log "len colors"

                        gradient =
                            colors
                                |> List.indexedMap
                                    (\i color ->
                                        color
                                            ++ " "
                                            ++ (toFloat i / toFloat len * 100 |> round |> toString)
                                            ++ "%,"
                                            ++ color
                                            ++ " "
                                            ++ (toFloat (i + 1) / toFloat len * 100 |> round |> toString)
                                            ++ "%"
                                    )
                                |> List.intersperse ","
                                |> String.concat
                    in
                        "linear-gradient(to bottom,"
                            ++ gradient
                            ++ ")"
          )
        ]


drawCursor { left, right, target } path origin =
    if target /= origin then
        []
    else if right == path then
        [ cursorStyleLeft, ( "margin-left", "-1px" ) ]
    else if left == path then
        [ cursorStyleRight ]
    else
        []


cursorAtEnd { left, right } lastPath =
    [ Html.span
        [ Html.style <|
            if lastPath == Nothing || Just left == lastPath then
                [ cursorStyleLeft ]
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
                        [ prettyInstanceUri peer.uri
                            |> text
                        ]
                    , column Version
                        [ width <| percent 10 ]
                        [ text <| toString peer.version ]
                    ]
        )
