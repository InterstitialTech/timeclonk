module ProjectListing exposing (..)

import Common
import Data
import Dialog as D
import Element as E exposing (Element)
import Element.Background as EBk
import Element.Border as EBd
import Element.Font as EF
import Element.Input as EI
import Element.Region
import TangoColors as TC
import TcCommon as TC
import Toop
import Util
import WindowKeys as WK


type Msg
    = SelectPress Int
    | NewPress
    | DonePress


type alias Model =
    { projects : List Data.ListProject
    }


type Command
    = Selected Int
    | New
    | Done
    | None


init : List Data.ListProject -> Model
init projects =
    { projects = projects }


view : Data.LoginData -> Util.Size -> Model -> Element Msg
view ld size model =
    let
        maxwidth =
            700

        titlemaxconst =
            85
    in
    E.el
        [ E.width E.fill
        , EBk.color TC.lightGrey
        ]
    <|
        E.column
            [ E.spacing 8
            , E.padding 8
            , E.width (E.maximum maxwidth E.fill)
            , E.centerX
            , EBk.color TC.lightGrey
            ]
            [ E.row [ E.spacing 8, E.width E.fill ]
                [ E.row [ EF.bold ] [ E.text ld.name ]
                , EI.button
                    (E.alignRight :: Common.buttonStyle)
                    { onPress = Just DonePress, label = E.text "settings" }
                ]
            , E.row [ E.spacing 8 ]
                [ EI.button Common.buttonStyle { onPress = Just NewPress, label = E.text "new" }
                ]
            , E.column
                [ E.padding 8
                , EBd.rounded 10
                , EBd.width 1
                , EBd.color TC.darkGrey
                , EBk.color TC.white
                , E.spacing 8
                ]
                [ E.table [ E.spacing 5, E.width E.fill, E.centerX ]
                    { data = model.projects
                    , columns =
                        [ { header = E.none
                          , width =
                                -- E.fill
                                -- clipX doesn't work unless max width is here in px, it seems.
                                -- E.px <| min maxwidth size.width - titlemaxconst
                                E.px <| min maxwidth size.width - 32
                          , view =
                                \n ->
                                    E.row
                                        [ E.centerY
                                        , E.clipX
                                        , E.width E.fill
                                        ]
                                        [ E.link
                                            [ E.height <| E.px 30 ]
                                            { url = TC.editProjectLink n.id
                                            , label = E.text n.name
                                            }
                                        ]
                          }
                        ]
                    }
                ]
            ]


update : Msg -> Model -> Data.LoginData -> ( Model, Command )
update msg model ld =
    case msg of
        SelectPress id ->
            ( model
            , Selected id
            )

        NewPress ->
            ( model, New )

        DonePress ->
            ( model, Done )
