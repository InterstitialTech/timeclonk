module UserSettings exposing (..)

import Common exposing (buttonStyle)
import Data
import Element as E exposing (Element)
import Element.Background as EBk
import Element.Border as EBd
import Element.Events as EE
import Element.Font as EF
import Element.Input as EI
import Orgauth.Data exposing (LoginData)
import TangoColors as TC
import TcCommon as TC


type Msg
    = Noop
    | DonePress
    | ChangePassPress
    | ChangeEmailPress
    | SetFontSize Int
    | SaveOnClonkChecked Bool
    | PageIncrementChanged String
    | LogOutPress


type Command
    = Done
    | LogOut
    | ChangePassword
    | ChangeEmail
    | ChangeFontSize Int
    | ChangeSaveOnClonk Bool
    | ChangePageIncrement Int
    | None


type alias Model =
    { login : LoginData
    , fontsize : Int
    , saveonclonk : Bool
    , pageincrement : Int
    , pageincrementstr : String
    }


init : LoginData -> Int -> Bool -> Int -> Model
init login fontsize saveonclonk pageincrement =
    { login = login
    , fontsize = fontsize
    , saveonclonk = saveonclonk
    , pageincrement = pageincrement
    , pageincrementstr = String.fromInt pageincrement
    }


view : Model -> Element Msg
view model =
    E.row [ E.width E.fill, E.height E.fill, EBk.color TC.lightGrey ]
        [ E.column
            [ E.centerX
            , E.width (E.maximum 400 E.fill)
            , E.spacing TC.defaultSpacing
            , E.alignTop
            ]
            [ E.row [ E.width E.fill ]
                [ EI.button buttonStyle { onPress = Just DonePress, label = E.text "done" }
                ]
            , E.column
                [ E.centerX
                , E.width (E.maximum 400 E.fill)
                , EBd.width 1
                , EBd.color TC.darkGrey
                , EBd.rounded 10
                , E.padding 5
                , EBk.color TC.white
                , E.spacing TC.defaultSpacing
                ]
                [ E.el [ EF.bold, E.centerX ] <| E.text "user settings"
                , E.row [ E.width E.fill ]
                    [ E.text "logged in as user: "
                    , E.el [ EF.bold ] <| E.text model.login.name
                    , EI.button (E.alignRight :: buttonStyle) { onPress = Just LogOutPress, label = E.text "log out" }
                    ]
                , EI.button (E.centerX :: buttonStyle) { onPress = Just ChangePassPress, label = E.text "change password" }
                , EI.button (E.centerX :: buttonStyle) { onPress = Just ChangeEmailPress, label = E.text "change email" }
                , EI.checkbox []
                    { onChange = SaveOnClonkChecked
                    , icon = EI.defaultCheckbox
                    , checked = model.saveonclonk
                    , label = EI.labelLeft [] <| E.text "Save on clonk in/out"
                    }
                , EI.text [ E.width <| E.px 100 ]
                    { onChange = PageIncrementChanged
                    , text = model.pageincrementstr
                    , placeholder = Nothing
                    , label = EI.labelLeft [] (E.text "page increment")
                    }
                , EI.slider
                    [ E.height (E.px 30)
                    , E.behindContent
                        (E.el
                            [ E.width E.fill
                            , E.height (E.px 2)
                            , E.centerY
                            , EBk.color TC.grey
                            , EBd.rounded 2
                            ]
                            E.none
                        )
                    ]
                    { onChange = round >> SetFontSize
                    , label =
                        EI.labelAbove []
                            (E.text "font size")
                    , min = 2
                    , max = 40
                    , step = Just 1
                    , value = model.fontsize |> toFloat
                    , thumb =
                        EI.defaultThumb
                    }
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Command )
update msg model =
    case msg of
        DonePress ->
            ( model, Done )

        LogOutPress ->
            ( model, LogOut )

        ChangePassPress ->
            ( model, ChangePassword )

        ChangeEmailPress ->
            ( model, ChangeEmail )

        SetFontSize size ->
            ( { model | fontsize = size }, ChangeFontSize size )

        SaveOnClonkChecked b ->
            ( { model | saveonclonk = b }, ChangeSaveOnClonk b )

        PageIncrementChanged str ->
            case String.toInt str of
                Just i ->
                    if model.pageincrement == i then
                        ( { model | pageincrementstr = str }, None )

                    else
                        ( { model | pageincrementstr = str, pageincrement = i }, ChangePageIncrement i )

                Nothing ->
                    ( { model | pageincrementstr = str }, None )

        Noop ->
            ( model, None )
