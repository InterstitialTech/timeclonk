module ProjectListing exposing (..)

import Common
import DataUtil
import Dialog as D
import Element as E exposing (Element)
import Element.Background as EBk
import Element.Border as EBd
import Element.Font as EF
import Element.Input as EI
import Element.Region
import Orgauth.Data as OD
import ProjectTime
import Route
import TangoColors as TC
import TcCommon as TC
import TcProtocol as TP
import Toop
import Util
import WindowKeys as WK


type Msg
    = NewPress
    | UserTimePress
    | DonePress
    | SettingsPress
    | AdminPress
    | InvitePress


type alias Model =
    { projects : List TP.ListProject
    }


type Command
    = New
    | UserTime
    | Done
    | Settings
    | Admin
    | Invite
    | None


init : List TP.ListProject -> Model
init projects =
    { projects = projects }


view : OD.AdminSettings -> DataUtil.LoginData -> Util.Size -> Model -> Element Msg
view adminSettings ld size model =
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
            [ E.spacing TC.defaultSpacing
            , E.padding 8
            , E.width (E.maximum maxwidth E.fill)
            , E.centerX
            , EBk.color TC.lightGrey
            ]
            [ E.row [ E.spacing TC.defaultSpacing, E.width E.fill ]
                [ E.row [ EF.bold ] [ E.text ld.name ]
                , if ld.admin then
                    EI.button
                        (E.alignRight :: Common.buttonStyle)
                        { onPress = Just AdminPress, label = E.text "admin" }

                  else if adminSettings.nonAdminInvite then
                    EI.button
                        (E.alignRight :: Common.buttonStyle)
                        { onPress = Just InvitePress, label = E.text "invite" }

                  else
                    E.none
                , EI.button
                    (E.alignRight :: Common.buttonStyle)
                    { onPress = Just SettingsPress, label = E.text "settings" }
                ]
            , E.row [ E.spacing TC.defaultSpacing, E.width E.fill ]
                [ EI.button Common.buttonStyle { onPress = Just NewPress, label = E.text "new" }
                , EI.button (E.alignRight :: Common.buttonStyle) { onPress = Just UserTimePress, label = E.text "all" }
                ]
            , E.column
                [ E.padding 8
                , EBd.rounded 10
                , EBd.width 1
                , EBd.color TC.darkGrey
                , EBk.color TC.white
                , E.spacing TC.defaultSpacing
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
                                            { url =
                                                Route.routeUrl
                                                    (Route.ProjectTimeR (DataUtil.getProjectIdVal n.id)
                                                        (ProjectTime.showViewMode ProjectTime.Clonks)
                                                    )
                                            , label = E.text n.name
                                            }
                                        ]
                          }
                        ]
                    }
                ]
            ]


update : Msg -> Model -> DataUtil.LoginData -> ( Model, Command )
update msg model ld =
    case msg of
        NewPress ->
            ( model, New )

        UserTimePress ->
            ( model, UserTime )

        DonePress ->
            ( model, Done )

        SettingsPress ->
            ( model, Settings )

        AdminPress ->
            ( model, Admin )

        InvitePress ->
            ( model, Invite )
