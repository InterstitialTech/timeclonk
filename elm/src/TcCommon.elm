module TcCommon exposing (..)

import Data as D
import Element as E exposing (Element)
import Element.Background as EBk
import Element.Border as EBd
import Element.Font as EF
import Element.Input as EI
import Html.Attributes
import TangoColors as TC
import Url.Builder as UB
import Util as U


type alias StylePalette =
    { defaultSpacing : Int
    }


myLinkStylePlain =
    [ EF.color TC.darkBlue ]


otherLinkStylePlain =
    [ EF.color TC.lightBlue ]


myLinkStyle =
    [ EF.color TC.black, EF.underline ]


otherLinkStyle =
    [ EF.color TC.darkBlue, EF.underline ]


saveLinkStyle =
    [ EF.color TC.darkYellow, EF.underline ]


fullScreen =
    E.row [ EF.size 10 ] [ E.column [] [ E.text "↖", E.text "↙" ], E.column [] [ E.text "↗", E.text "↘" ] ]


defaultSpacing : Int
defaultSpacing =
    10


checkboxIcon : Bool -> Element msg
checkboxIcon checked =
    E.el
        [ E.htmlAttribute <| Html.Attributes.class "focusable"
        , E.width
            (E.px 14)
        , E.height (E.px 14)
        , EF.color TC.white
        , E.centerY
        , EF.size 9
        , EF.center
        , EBd.rounded 3
        , EBd.color <|
            if checked then
                TC.darkBlue

            else
                E.rgb (211 / 255) (211 / 255) (211 / 255)
        , EBd.shadow
            { offset = ( 0, 0 )
            , blur = 1
            , size = 1
            , color =
                if checked then
                    E.rgba (238 / 255) (238 / 255) (238 / 255) 0

                else
                    TC.darkBlue
            }
        , EBk.color <|
            if checked then
                TC.darkBlue

            else
                TC.white
        , EBd.width <|
            if checked then
                0

            else
                1
        , E.inFront
            (E.el
                [ EBd.color TC.white
                , E.height (E.px 6)
                , E.width (E.px 9)
                , E.rotate (degrees -45)
                , E.centerX
                , E.centerY
                , E.moveUp 1
                , E.transparent (not checked)
                , EBd.widthEach
                    { top = 0
                    , left = 2
                    , bottom = 2
                    , right = 0
                    }
                ]
                E.none
            )
        ]
        E.none
