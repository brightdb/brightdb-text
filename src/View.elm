module View exposing (view)

import Types exposing (..)
import Html exposing (Html)
import Html.Attributes as Html
import Color
import Style exposing (..)
import Style.Font as Font
import Style.Border as Border
import Style.Color as Color
import Element exposing (..)
import Element.Input as Input
import Element.Attributes exposing (..)
import Element.Events exposing (..)

type Styles
    = None
    | Main
    | Navbar
    | Content
    | Title
    | Peer

type Variations
    = Greyed

navbarColor =
    Color.rgb 233 240 248

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
        ]

view : Model -> Html Msg
view model =
    Element.viewport stylesheet <|
        column None
          [ height <| fill
          ]
          [ row
              Navbar
                [ width <| fill
                , padding 20
                ]
                [ text "Demo"
                ]
          , row None
                [ width <| fill
                , height <| fill
                , scrollbars
                ]
                [ content model, sidebar model ]
          ]

sidebar model =
    column None
        [ padding 10
        , spacing 10
        ]
        ( peers model.peers
        )

content model =
    column Content
        [ alignLeft
        , width <| fill
        , scrollbars
        ]
        []


peers =
  List.map
    (\peer ->
      row Peer
        []
        [ text peer ]
    )
