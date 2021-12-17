module TcCommon exposing (..)

import Data as D
import Element as E exposing (Element)
import Element.Background as EBk
import Element.Border as EBd
import Element.Font as EF
import Element.Input as EI
import TangoColors as TC
import Url.Builder as UB
import Util as U


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
