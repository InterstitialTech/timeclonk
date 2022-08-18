port module Main exposing (main)

import Array
import Browser
import Browser.Events
import Browser.Navigation
import Common exposing (buttonStyle)
import Data
import Dict exposing (Dict)
import DisplayMessage
import Element as E exposing (Element)
import Element.Background as EBk
import Element.Border as EBd
import Element.Font as EF
import Element.Input as EI
import Element.Region
import File as F
import File.Download as FD
import File.Select as FS
import GenDialog as GD
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events as HE
import Http
import Json.Decode as JD
import Json.Encode as JE
import LocalStorage as LS
import Orgauth.AdminInterface as AI
import Orgauth.ChangeEmail as CE
import Orgauth.ChangePassword as CP
import Orgauth.Data as OD exposing (AdminSettings, UserId, getUserIdVal, makeUserId)
import Orgauth.Invited as Invited
import Orgauth.Login as Login
import Orgauth.ResetPassword as ResetPassword
import Orgauth.UserEdit as UserEdit
import Orgauth.UserInterface as UI
import Orgauth.UserInvite as UserInvite
import Orgauth.UserListing as UserListing
import ProjectEdit
import ProjectListing
import ProjectTime
import ProjectView
import PublicInterface as PI
import Random exposing (Seed, initialSeed)
import Route exposing (Route(..), parseUrl, routeTitle, routeUrl)
import SelectString as SS
import ShowMessage
import TDict exposing (TDict)
import TangoColors as TC
import Task exposing (Task)
import TcCommon
import Time
import TimeReporting as TR
import TimeclonkInterface as TI
import Toop
import UUID exposing (UUID)
import Url exposing (Url)
import Url.Builder as UB
import Url.Parser as UP exposing ((</>))
import UserSettings
import Util
import WindowKeys


type Msg
    = LoginMsg Login.Msg
    | InvitedMsg Invited.Msg
    | UserSettingsMsg UserSettings.Msg
    | UserEditMsg UserEdit.Msg
    | UserListingMsg UserListing.Msg
    | UserInviteMsg UserInvite.Msg
    | ShowMessageMsg ShowMessage.Msg
    | UserReplyData (Result Http.Error UI.ServerResponse)
    | AdminReplyData (Result Http.Error AI.ServerResponse)
    | TimeclonkReplyData (Result Http.Error TI.ServerResponse)
    | PublicReplyData (Result Http.Error PI.ServerResponse)
    | ProjectTimeData String (Result Http.Error TI.ServerResponse)
    | ProjectViewData String (Result Http.Error PI.ServerResponse)
    | TProjectViewData String (Result Http.Error TI.ServerResponse)
    | LoadUrl String
    | InternalUrl Url
    | SelectedText JD.Value
    | UrlChanged Url
    | WindowSize Util.Size
    | DisplayMessageMsg (GD.Msg DisplayMessage.Msg)
    | SelectUserDialogMsg (GD.Msg (SS.Msg Data.User))
    | SelectRoleDialogMsg (GD.Msg (SS.Msg ( UserId, Data.Role )))
    | ChangePasswordDialogMsg (GD.Msg CP.Msg)
    | ChangeEmailDialogMsg (GD.Msg CE.Msg)
    | ResetPasswordMsg ResetPassword.Msg
    | Zone Time.Zone
    | WkMsg (Result JD.Error WindowKeys.Key)
    | ReceiveLocalVal { for : String, name : String, value : Maybe String }
    | ClockTick Time.Posix
    | ProjectListingMsg ProjectListing.Msg
    | ProjectViewMsg ProjectView.Msg
    | ProjectEditMsg ProjectEdit.Msg
    | ProjectTimeMsg ProjectTime.Msg
    | FileLoaded (String -> Msg) F.File
    | Noop


type State
    = Login Login.Model
    | Invited Invited.Model
    | UserSettings UserSettings.Model Data.LoginData State
    | UserListing UserListing.Model Data.LoginData
    | UserEdit UserEdit.Model Data.LoginData
    | UserInvite UserInvite.Model Data.LoginData
    | ShowMessage ShowMessage.Model Data.LoginData (Maybe State)
    | PubShowMessage ShowMessage.Model (Maybe State)
    | LoginShowMessage ShowMessage.Model Data.LoginData Url
    | SelectUserDialog (SS.GDModel Data.User) State
    | SelectRoleDialog (SS.GDModel ( UserId, Data.Role )) State
    | ChangePasswordDialog CP.GDModel State
    | ChangeEmailDialog CE.GDModel State
    | ResetPassword ResetPassword.Model
    | DisplayMessage DisplayMessage.GDModel State
    | Wait State (Model -> Msg -> ( Model, Cmd Msg ))
    | ProjectListing ProjectListing.Model Data.LoginData
    | ProjectEdit ProjectEdit.Model Data.LoginData
    | ProjectView ProjectView.Model (Maybe Data.LoginData)
    | ProjectTime ProjectTime.Model Data.LoginData


type alias Flags =
    { seed : Int
    , location : String
    , useragent : String
    , appname : String
    , debugstring : String
    , width : Int
    , height : Int
    , login : Maybe JD.Value
    , adminsettings : Maybe JD.Value
    }


type alias SavedRoute =
    { route : Route
    , save : Bool
    }


type alias Model =
    { state : State
    , size : Util.Size
    , location : String
    , appname : String
    , navkey : Browser.Navigation.Key
    , seed : Seed
    , timezone : Time.Zone
    , savedRoute : SavedRoute
    , fontsize : Int
    , saveonclonk : Bool
    , pageincrement : Int
    , stylePalette : TcCommon.StylePalette
    , adminSettings : AdminSettings
    }


type alias PreInitModel =
    { flags : Flags
    , url : Url
    , key : Browser.Navigation.Key
    , mbzone : Maybe Time.Zone
    , mbfontsize : Maybe Int
    , mbsaveonclonk : Maybe Bool
    , mbpageincrement : Maybe Int
    }


type PiModel
    = Ready Model
    | PreInit PreInitModel


initLoginState : Model -> State
initLoginState model =
    initLoginState model


urlRequest : Browser.UrlRequest -> Msg
urlRequest ur =
    case ur of
        Browser.Internal url ->
            InternalUrl url

        Browser.External str ->
            LoadUrl str


routeState : Model -> Route -> ( State, Cmd Msg )
routeState model route =
    case route of
        LoginR ->
            ( initLoginState model, Cmd.none )

        ResetPasswordR username key ->
            ( ResetPassword <| ResetPassword.initialModel username key model.appname, Cmd.none )

        SettingsR ->
            case stateLogin model.state of
                Just login ->
                    ( UserSettings (UserSettings.init (Data.ldToOdLd login) model.fontsize model.saveonclonk model.pageincrement) login model.state, Cmd.none )

                Nothing ->
                    ( (displayMessageDialog { model | state = initLoginState model } "can't view user settings; you're not logged in!").state, Cmd.none )

        Top ->
            if (stateRoute model.state).route == Top then
                ( model.state, Cmd.none )

            else
                -- home page if any, or login page if not logged in.
                let
                    ( m, c ) =
                        initialPage model
                in
                ( m.state, c )

        ProjectEditR id ->
            ( (displayMessageDialog model "loading project").state
            , sendTIMsg model.location <| TI.GetProjectEdit id
            )

        ProjectTimeR id mode ->
            ( (displayMessageDialog model "loading project").state
            , sendTIMsgExp model.location (TI.GetProjectTime id) (ProjectTimeData mode)
            )

        ProjectViewR id mode ->
            case stateLogin model.state of
                Just login ->
                    ( (displayMessageDialog model "loading project").state
                    , sendTIMsgExp model.location (TI.GetProjectTime id) (TProjectViewData mode)
                    )

                Nothing ->
                    ( (displayMessageDialog model "loading project").state
                    , sendPIMsgExp model.location (PI.GetProjectTime id) (ProjectViewData mode)
                    )


stateRoute : State -> SavedRoute
stateRoute state =
    case state of
        Login _ ->
            { route = LoginR
            , save = False
            }

        UserSettings _ _ _ ->
            { route = SettingsR
            , save = True
            }

        ResetPassword mod ->
            { route = Top
            , save = False
            }

        ProjectEdit mod _ ->
            { route =
                case mod.id of
                    Just pid ->
                        ProjectEditR (Data.getProjectIdVal pid)

                    Nothing ->
                        Top
            , save = True
            }

        ProjectTime mod _ ->
            { route = ProjectTimeR (Data.getProjectIdVal mod.project.id) (ProjectTime.showViewMode mod.viewmode)
            , save = True
            }

        ProjectView mod _ ->
            { route = ProjectViewR (Data.getProjectIdVal mod.project.id) (ProjectView.showViewMode mod.viewmode)
            , save = True
            }

        _ ->
            { route = Top
            , save = False
            }


showMessage : Msg -> String
showMessage msg =
    case msg of
        LoginMsg _ ->
            "LoginMsg"

        InvitedMsg _ ->
            "InvitedMsg"

        UserEditMsg _ ->
            "UserEditMsg"

        UserListingMsg _ ->
            "UserListingMsg"

        UserInviteMsg _ ->
            "UserInviteMsg"

        DisplayMessageMsg _ ->
            "DisplayMessage"

        UserSettingsMsg _ ->
            "UserSettingsMsg"

        ShowMessageMsg _ ->
            "ShowMessageMsg"

        UserReplyData urd ->
            "UserReplyData: "
                ++ (Result.map UI.showServerResponse urd
                        |> Result.mapError Util.httpErrorString
                        |> (\r ->
                                case r of
                                    Ok m ->
                                        "message: " ++ m

                                    Err e ->
                                        "error: " ++ e
                           )
                   )

        AdminReplyData urd ->
            "AdminReplyData: "
                ++ (Result.map AI.showServerResponse urd
                        |> Result.mapError Util.httpErrorString
                        |> (\r ->
                                case r of
                                    Ok m ->
                                        "message: " ++ m

                                    Err e ->
                                        "error: " ++ e
                           )
                   )

        TimeclonkReplyData urd ->
            "TimeclonkReplyData: "
                ++ (Result.map TI.showServerResponse urd
                        |> Result.mapError Util.httpErrorString
                        |> (\r ->
                                case r of
                                    Ok m ->
                                        "message: " ++ m

                                    Err e ->
                                        "error: " ++ e
                           )
                   )

        PublicReplyData urd ->
            "PublicReplyData: "
                ++ (Result.map PI.showServerResponse urd
                        |> Result.mapError Util.httpErrorString
                        |> (\r ->
                                case r of
                                    Ok m ->
                                        "message: " ++ m

                                    Err e ->
                                        "error: " ++ e
                           )
                   )

        ProjectTimeData mode urd ->
            "ProjectTimeData: "
                ++ (Result.map TI.showServerResponse urd
                        |> Result.mapError Util.httpErrorString
                        |> (\r ->
                                case r of
                                    Ok m ->
                                        "message: " ++ m

                                    Err e ->
                                        "error: " ++ e
                           )
                   )
                ++ "\nmode: "
                ++ mode

        ProjectViewData mode urd ->
            "ProjectViewData: "
                ++ (Result.map PI.showServerResponse urd
                        |> Result.mapError Util.httpErrorString
                        |> (\r ->
                                case r of
                                    Ok m ->
                                        "message: " ++ m

                                    Err e ->
                                        "error: " ++ e
                           )
                   )
                ++ "\nmode: "
                ++ mode

        TProjectViewData mode urd ->
            "TProjectViewData: "
                ++ (Result.map TI.showServerResponse urd
                        |> Result.mapError Util.httpErrorString
                        |> (\r ->
                                case r of
                                    Ok m ->
                                        "message: " ++ m

                                    Err e ->
                                        "error: " ++ e
                           )
                   )
                ++ "\nmode: "
                ++ mode

        LoadUrl _ ->
            "LoadUrl"

        InternalUrl _ ->
            "InternalUrl"

        SelectedText _ ->
            "SelectedText"

        UrlChanged _ ->
            "UrlChanged"

        WindowSize _ ->
            "WindowSize"

        FileLoaded _ _ ->
            "FileLoaded"

        Noop ->
            "Noop"

        WkMsg _ ->
            "WkMsg"

        ReceiveLocalVal _ ->
            "ReceiveLocalVal"

        SelectUserDialogMsg _ ->
            "SelectUserDialogMsg"

        SelectRoleDialogMsg _ ->
            "SelectRoleDialogMsg"

        ChangePasswordDialogMsg _ ->
            "ChangePasswordDialogMsg"

        ChangeEmailDialogMsg _ ->
            "ChangeEmailDialogMsg"

        ResetPasswordMsg _ ->
            "ResetPasswordMsg"

        Zone _ ->
            "Zone"

        ProjectListingMsg _ ->
            "ProjectListingMsg"

        ProjectViewMsg _ ->
            "ProjectViewMsg"

        ProjectEditMsg _ ->
            "ProjectEditMsg"

        ProjectTimeMsg _ ->
            "ProjectTimeMsg"

        ClockTick _ ->
            "ClockTick"


showState : State -> String
showState state =
    case state of
        Login _ ->
            "Login"

        Invited _ ->
            "Invited"

        UserEdit _ _ ->
            "UserEdit"

        UserListing _ _ ->
            "UserListing"

        UserInvite _ _ ->
            "UserInvite"

        UserSettings _ _ _ ->
            "UserSettings"

        DisplayMessage gdm _ ->
            "DisplayMessage: " ++ gdm.model.message

        ShowMessage _ _ _ ->
            "ShowMessage"

        PubShowMessage _ _ ->
            "PubShowMessage"

        LoginShowMessage _ _ _ ->
            "LoginShowMessage"

        Wait _ _ ->
            "Wait"

        SelectUserDialog _ _ ->
            "SelectDialog"

        SelectRoleDialog _ _ ->
            "SelectRoleDialog"

        ChangePasswordDialog _ _ ->
            "ChangePasswordDialog"

        ChangeEmailDialog _ _ ->
            "ChangeEmailDialog"

        ResetPassword _ ->
            "ResetPassword"

        ProjectListing _ _ ->
            "ProjectListing"

        ProjectEdit _ _ ->
            "ProjectEdit"

        ProjectTime _ _ ->
            "ProjectTime"

        ProjectView _ _ ->
            "ProjectView"


unexpectedMsg : Model -> Msg -> Model
unexpectedMsg model msg =
    unexpectedMessage model (showMessage msg)


unexpectedMessage : Model -> String -> Model
unexpectedMessage model msg =
    displayMessageDialog model
        ("unexpected message - " ++ msg ++ "; state was " ++ showState model.state)


viewState : Util.Size -> State -> Model -> Element Msg
viewState size state model =
    case state of
        Login lem ->
            E.map LoginMsg <| Login.view model.stylePalette size lem

        Invited em ->
            E.map InvitedMsg <| Invited.view model.stylePalette size em

        UserEdit em login ->
            E.map UserEditMsg <| UserEdit.view [] em

        UserListing em login ->
            E.map UserListingMsg <| UserListing.view [] em

        UserInvite em login ->
            E.map UserInviteMsg <| UserInvite.view [] em

        ShowMessage em _ _ ->
            E.map ShowMessageMsg <| ShowMessage.view em

        PubShowMessage em _ ->
            E.map ShowMessageMsg <| ShowMessage.view em

        LoginShowMessage em _ _ ->
            E.map ShowMessageMsg <| ShowMessage.view em

        UserSettings em _ _ ->
            E.map UserSettingsMsg <| UserSettings.view em

        DisplayMessage em _ ->
            -- render is at the layout level, not here.
            E.none

        Wait innerState _ ->
            E.map (\_ -> Noop) (viewState size innerState model)

        SelectUserDialog _ _ ->
            -- render is at the layout level, not here.
            E.none

        SelectRoleDialog _ _ ->
            -- render is at the layout level, not here.
            E.none

        ChangePasswordDialog _ _ ->
            -- render is at the layout level, not here.
            E.none

        ChangeEmailDialog _ _ ->
            -- render is at the layout level, not here.
            E.none

        ResetPassword st ->
            E.map ResetPasswordMsg (ResetPassword.view size st)

        ProjectListing em ld ->
            E.map ProjectListingMsg <| ProjectListing.view ld size em

        ProjectEdit em ld ->
            E.map ProjectEditMsg <| ProjectEdit.view ld size em

        ProjectTime em ld ->
            E.map ProjectTimeMsg <| ProjectTime.view ld size model.timezone em

        ProjectView em ld ->
            E.map ProjectViewMsg <| ProjectView.view (Util.isJust ld) size model.timezone em


stateLogin : State -> Maybe Data.LoginData
stateLogin state =
    case state of
        Login _ ->
            Nothing

        UserSettings _ login _ ->
            Just login

        UserEdit _ login ->
            Just login

        UserListing _ login ->
            Just login

        UserInvite _ login ->
            Just login

        DisplayMessage _ bestate ->
            stateLogin bestate

        ShowMessage _ login _ ->
            Just login

        PubShowMessage _ _ ->
            Nothing

        LoginShowMessage _ _ _ ->
            Nothing

        Wait wstate _ ->
            stateLogin wstate

        SelectUserDialog _ instate ->
            stateLogin instate

        SelectRoleDialog _ instate ->
            stateLogin instate

        ChangePasswordDialog _ instate ->
            stateLogin instate

        ChangeEmailDialog _ instate ->
            stateLogin instate

        ResetPassword _ ->
            Nothing

        ProjectListing _ login ->
            Just login

        ProjectEdit _ login ->
            Just login

        ProjectTime _ login ->
            Just login

        ProjectView _ mblogin ->
            mblogin

        Invited _ ->
            Nothing


sendTIMsg : String -> TI.SendMsg -> Cmd Msg
sendTIMsg location msg =
    sendTIMsgExp location msg TimeclonkReplyData


sendTIMsgExp : String -> TI.SendMsg -> (Result Http.Error TI.ServerResponse -> Msg) -> Cmd Msg
sendTIMsgExp location msg tomsg =
    Http.post
        { url = location ++ "/private"
        , body = Http.jsonBody (TI.encodeSendMsg msg)
        , expect = Http.expectJson tomsg TI.serverResponseDecoder
        }


sendPIMsg : String -> PI.SendMsg -> Cmd Msg
sendPIMsg location msg =
    sendPIMsgExp location msg PublicReplyData


sendPIMsgExp : String -> PI.SendMsg -> (Result Http.Error PI.ServerResponse -> Msg) -> Cmd Msg
sendPIMsgExp location msg tomsg =
    Http.post
        { url = location ++ "/public"
        , body = Http.jsonBody (PI.encodeSendMsg msg)
        , expect = Http.expectJson tomsg PI.serverResponseDecoder
        }


sendUIMsg : String -> UI.SendMsg -> Cmd Msg
sendUIMsg location msg =
    sendUIMsgExp location msg UserReplyData


sendUIMsgExp : String -> UI.SendMsg -> (Result Http.Error UI.ServerResponse -> Msg) -> Cmd Msg
sendUIMsgExp location msg tomsg =
    Http.post
        { url = location ++ "/user"
        , body = Http.jsonBody (UI.encodeSendMsg msg)
        , expect = Http.expectJson tomsg UI.serverResponseDecoder
        }


sendAIMsg : String -> AI.SendMsg -> Cmd Msg
sendAIMsg location msg =
    sendAIMsgExp location msg AdminReplyData


sendAIMsgExp : String -> AI.SendMsg -> (Result Http.Error AI.ServerResponse -> Msg) -> Cmd Msg
sendAIMsgExp location msg tomsg =
    Http.post
        { url = location ++ "/admin"
        , body = Http.jsonBody (AI.encodeSendMsg msg)
        , expect = Http.expectJson tomsg AI.serverResponseDecoder
        }


piview : PiModel -> { title : String, body : List (Html Msg) }
piview pimodel =
    case pimodel of
        Ready model ->
            view model

        PreInit model ->
            { title = model.flags.appname ++ ": initializing"
            , body = []
            }


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title =
        routeTitle model.appname model.savedRoute.route
    , body =
        [ case model.state of
            DisplayMessage dm _ ->
                Html.map DisplayMessageMsg <|
                    GD.layout
                        (Just { width = min 600 model.size.width, height = min 500 model.size.height })
                        dm

            SelectUserDialog sdm _ ->
                Html.map SelectUserDialogMsg <|
                    GD.layout
                        (Just { width = min 600 model.size.width, height = min 500 model.size.height })
                        sdm

            SelectRoleDialog sdm _ ->
                Html.map SelectRoleDialogMsg <|
                    GD.layout
                        (Just { width = min 600 model.size.width, height = min 500 model.size.height })
                        sdm

            ChangePasswordDialog cdm _ ->
                Html.map ChangePasswordDialogMsg <|
                    GD.layout
                        (Just { width = min 600 model.size.width, height = min 200 model.size.height })
                        cdm

            ChangeEmailDialog cdm _ ->
                Html.map ChangeEmailDialogMsg <|
                    GD.layout
                        (Just { width = min 600 model.size.width, height = min 200 model.size.height })
                        cdm

            _ ->
                E.layout [ EF.size model.fontsize, E.width E.fill ] <| viewState model.size model.state model
        ]
    }


piupdate : Msg -> PiModel -> ( PiModel, Cmd Msg )
piupdate msg initmodel =
    case initmodel of
        Ready model ->
            let
                ( m, c ) =
                    urlupdate msg model
            in
            ( Ready m, c )

        PreInit imod ->
            let
                nmod =
                    case msg of
                        Zone zone ->
                            { imod | mbzone = Just zone }

                        ReceiveLocalVal lv ->
                            let
                                default =
                                    16

                                defaultsaveonclonk =
                                    True

                                defaultpageincrement =
                                    25
                            in
                            case lv.name of
                                "fontsize" ->
                                    case lv.value of
                                        Just v ->
                                            case String.toInt v of
                                                Just i ->
                                                    { imod | mbfontsize = Just i }

                                                Nothing ->
                                                    { imod | mbfontsize = Just default }

                                        Nothing ->
                                            { imod | mbfontsize = Just default }

                                "saveonclonk" ->
                                    case lv.value of
                                        Just v ->
                                            case String.toInt v of
                                                Just i ->
                                                    { imod | mbsaveonclonk = Just (i == 1) }

                                                Nothing ->
                                                    { imod | mbsaveonclonk = Just defaultsaveonclonk }

                                        Nothing ->
                                            { imod | mbsaveonclonk = Just defaultsaveonclonk }

                                "pageincrement" ->
                                    case lv.value of
                                        Just v ->
                                            case String.toInt v of
                                                Just i ->
                                                    { imod | mbpageincrement = Just i }

                                                Nothing ->
                                                    { imod | mbpageincrement = Just defaultpageincrement }

                                        Nothing ->
                                            { imod | mbpageincrement = Just defaultpageincrement }

                                _ ->
                                    { imod | mbfontsize = Nothing }

                        _ ->
                            imod
            in
            case Toop.T4 nmod.mbzone nmod.mbfontsize nmod.mbsaveonclonk nmod.mbpageincrement of
                Toop.T4 (Just zone) (Just fontsize) (Just saveonclonk) (Just pageincrement) ->
                    let
                        ( m, c ) =
                            init imod.flags imod.url imod.key zone fontsize saveonclonk pageincrement
                    in
                    ( Ready m, c )

                _ ->
                    ( PreInit nmod, Cmd.none )


{-| urlUpdate: all URL code shall go here! regular code shall not worry about urls!
this function calls actualupdate where the app stuff happens.
url messages and state based url changes are done here.
-}
urlupdate : Msg -> Model -> ( Model, Cmd Msg )
urlupdate msg model =
    let
        ( nm, cmd ) =
            case msg of
                InternalUrl url ->
                    let
                        ( state, icmd ) =
                            parseUrl url
                                |> Maybe.map (routeState model)
                                |> Maybe.withDefault ( model.state, Cmd.none )
                    in
                    ( { model | state = state }, icmd )

                LoadUrl urlstr ->
                    -- load foreign site
                    -- ( model, Browser.Navigation.load urlstr )
                    ( model, Cmd.none )

                UrlChanged url ->
                    -- we get this from forward and back buttons.  if the user changes the url
                    -- in the browser address bar, its a site reload so this isn't called.
                    case parseUrl url of
                        Just route ->
                            if route == (stateRoute model.state).route then
                                ( model, Cmd.none )

                            else
                                let
                                    ( st, rscmd ) =
                                        routeState model route
                                in
                                -- swap out the savedRoute, so we don't write over history.
                                ( { model
                                    | state = st
                                    , savedRoute =
                                        let
                                            nssr =
                                                stateRoute st
                                        in
                                        { nssr | save = False }
                                  }
                                , rscmd
                                )

                        Nothing ->
                            -- load foreign site
                            -- ( model, Browser.Navigation.load (Url.toString url) )
                            ( model, Cmd.none )

                _ ->
                    -- not an url related message!  pass it on to the 'actualupdate'
                    -- this is where all the app stuff happens.
                    actualupdate msg model

        sr =
            stateRoute nm.state
    in
    -- when the route changes, change the address bar, optionally pushing what's there to
    -- browser history.
    if sr.route /= nm.savedRoute.route then
        ( { nm | savedRoute = sr }
        , if model.savedRoute.save then
            Cmd.batch
                [ cmd
                , Browser.Navigation.pushUrl nm.navkey
                    (routeUrl sr.route)
                ]

          else
            Cmd.batch
                [ cmd
                , Browser.Navigation.replaceUrl nm.navkey
                    (routeUrl sr.route)
                ]
        )

    else
        ( nm, cmd )


displayMessageDialog : Model -> String -> Model
displayMessageDialog model message =
    { model
        | state =
            DisplayMessage
                (DisplayMessage.init Common.buttonStyle
                    message
                    (E.map (\_ -> ()) (viewState model.size model.state model))
                )
                model.state
    }


openProjectTime : Model -> String -> Data.ProjectTime -> ( Model, Cmd Msg )
openProjectTime model mode pt =
    case stateLogin model.state of
        Just login ->
            let
                mbrole =
                    List.foldl
                        (\m mbr ->
                            if m.id == login.userid then
                                Just m.role

                            else
                                mbr
                        )
                        Nothing
                        pt.members
            in
            let
                obs =
                    case mbrole of
                        Just Data.Observer ->
                            True

                        Just Data.Member ->
                            False

                        Just Data.Admin ->
                            False

                        Nothing ->
                            True
            in
            if obs then
                ( { model | state = ProjectView (ProjectView.init model.timezone pt model.pageincrement mode) (Just login) }, Cmd.none )

            else
                ( { model | state = ProjectTime (ProjectTime.init model.timezone login pt model.saveonclonk model.pageincrement mode) login }, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


actualupdate : Msg -> Model -> ( Model, Cmd Msg )
actualupdate msg model =
    case ( msg, model.state ) of
        ( _, Wait wst wfn ) ->
            let
                ( nmd, cmd ) =
                    wfn model msg
            in
            ( nmd, cmd )

        ( ReceiveLocalVal lv, _ ) ->
            -- update the font size
            ( model, Cmd.none )

        ( ClockTick posix, state ) ->
            case state of
                ProjectTime pt login ->
                    ( { model | state = ProjectTime (ProjectTime.onClockTick posix pt) login }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ( WindowSize s, _ ) ->
            ( { model | size = s }, Cmd.none )

        ( ChangePasswordDialogMsg sdmsg, ChangePasswordDialog sdmod instate ) ->
            case GD.update sdmsg sdmod of
                GD.Dialog nmod ->
                    ( { model | state = ChangePasswordDialog nmod instate }, Cmd.none )

                GD.Ok return ->
                    ( { model | state = instate }
                    , sendUIMsg model.location <| UI.ChangePassword return
                    )

                GD.Cancel ->
                    ( { model | state = instate }, Cmd.none )

        ( ChangeEmailDialogMsg sdmsg, ChangeEmailDialog sdmod instate ) ->
            case GD.update sdmsg sdmod of
                GD.Dialog nmod ->
                    ( { model | state = ChangeEmailDialog nmod instate }, Cmd.none )

                GD.Ok return ->
                    ( { model | state = instate }
                    , sendUIMsg model.location <| UI.ChangeEmail return
                    )

                GD.Cancel ->
                    ( { model | state = instate }, Cmd.none )

        ( ResetPasswordMsg rmsg, ResetPassword rst ) ->
            let
                ( nst, cmd ) =
                    ResetPassword.update rmsg rst
            in
            case cmd of
                ResetPassword.Ok ->
                    ( { model | state = ResetPassword nst }
                    , sendUIMsg model.location
                        (UI.SetPassword { uid = nst.userId, newpwd = nst.password, reset_key = nst.reset_key })
                    )

                ResetPassword.None ->
                    ( { model | state = ResetPassword nst }, Cmd.none )

        ( UserSettingsMsg umsg, UserSettings umod login prevstate ) ->
            let
                ( numod, c ) =
                    UserSettings.update umsg umod
            in
            case c of
                UserSettings.Done ->
                    case prevstate of
                        ProjectTime ptm ld ->
                            let
                                nptm =
                                    { ptm | saveonclonk = numod.saveonclonk }
                                        |> ProjectTime.setPageIncrement numod.pageincrement
                            in
                            ( { model | state = ProjectTime nptm ld }, Cmd.none )

                        ShowMessage _ logindata Nothing ->
                            initialPage model

                        PubShowMessage _ Nothing ->
                            initialPage model

                        _ ->
                            ( { model | state = prevstate }, Cmd.none )

                UserSettings.LogOut ->
                    ( { model | state = initLoginState model }
                    , sendUIMsg model.location UI.Logout
                    )

                UserSettings.ChangePassword ->
                    ( { model
                        | state =
                            ChangePasswordDialog (CP.init (Data.ldToOdLd login) Common.buttonStyle (UserSettings.view numod |> E.map (always ())))
                                (UserSettings numod login prevstate)
                      }
                    , Cmd.none
                    )

                UserSettings.ChangeEmail ->
                    ( { model
                        | state =
                            ChangeEmailDialog (CE.init (Data.ldToOdLd login) Common.buttonStyle (UserSettings.view numod |> E.map (always ())))
                                (UserSettings numod login prevstate)
                      }
                    , Cmd.none
                    )

                UserSettings.ChangeFontSize size ->
                    ( { model
                        | state = UserSettings numod login prevstate
                        , fontsize = size
                      }
                    , LS.storeLocalVal { name = "fontsize", value = String.fromInt size }
                    )

                UserSettings.ChangePageIncrement i ->
                    ( { model
                        | state = UserSettings numod login prevstate
                        , pageincrement = i
                      }
                    , LS.storeLocalVal { name = "pageincrement", value = String.fromInt i }
                    )

                UserSettings.ChangeSaveOnClonk b ->
                    ( { model
                        | state = UserSettings numod login prevstate
                        , saveonclonk = b
                      }
                    , LS.storeLocalVal
                        { name = "saveonclonk"
                        , value =
                            String.fromInt
                                (if b then
                                    1

                                 else
                                    0
                                )
                        }
                    )

                UserSettings.None ->
                    ( { model | state = UserSettings numod login prevstate }, Cmd.none )

        ( WkMsg rkey, Login ls ) ->
            case rkey of
                Ok key ->
                    handleLogin model (Login.onWkKeyPress key ls)

                Err _ ->
                    ( model, Cmd.none )

        ( LoginMsg lm, Login ls ) ->
            handleLogin model (Login.update lm ls)

        ( InvitedMsg lm, Invited ls ) ->
            handleInvited model (Invited.update lm ls)

        ( FileLoaded toMsg file, _ ) ->
            ( model
            , Task.perform toMsg (F.toString file)
            )

        ( ProjectTimeData mode urd, state ) ->
            case urd of
                Err e ->
                    ( displayMessageDialog model <| Util.httpErrorString e, Cmd.none )

                Ok uiresponse ->
                    case uiresponse of
                        TI.ProjectTime x ->
                            openProjectTime model mode x

                        TI.NotLoggedIn ->
                            ( { model | state = initLoginState model }, Cmd.none )

                        TI.InvalidUserOrPwd ->
                            ( { model | state = initLoginState model }, Cmd.none )

                        _ ->
                            ( unexpectedMsg model msg
                            , Cmd.none
                            )

        ( ProjectViewData mode urd, state ) ->
            case urd of
                Err e ->
                    ( displayMessageDialog model <| Util.httpErrorString e, Cmd.none )

                Ok uiresponse ->
                    case uiresponse of
                        PI.ProjectTime x ->
                            ( { model
                                | state =
                                    ProjectView
                                        (ProjectView.init model.timezone x model.pageincrement mode)
                                        (stateLogin state)
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( unexpectedMsg model msg
                            , Cmd.none
                            )

        ( TProjectViewData mode urd, state ) ->
            case urd of
                Err e ->
                    ( displayMessageDialog model <| Util.httpErrorString e, Cmd.none )

                Ok uiresponse ->
                    case uiresponse of
                        TI.ProjectTime x ->
                            ( { model
                                | state =
                                    ProjectView
                                        (ProjectView.init model.timezone x model.pageincrement mode)
                                        (stateLogin state)
                              }
                            , Cmd.none
                            )

                        TI.NotLoggedIn ->
                            ( { model | state = initLoginState model }, Cmd.none )

                        _ ->
                            ( unexpectedMsg model msg
                            , Cmd.none
                            )

        ( UserReplyData urd, state ) ->
            case urd of
                Err e ->
                    ( displayMessageDialog model <| Util.httpErrorString e, Cmd.none )

                Ok uiresponse ->
                    case uiresponse of
                        UI.ServerError e ->
                            ( displayMessageDialog model <| e, Cmd.none )

                        UI.RegistrationSent ->
                            ( model, Cmd.none )

                        UI.LoggedIn login ->
                            let
                                lgmod =
                                    { model
                                        | state =
                                            ShowMessage { message = "logged in" }
                                                (Data.odLdToLd login)
                                                Nothing
                                    }
                            in
                            case state of
                                Login lm ->
                                    -- we're logged in!
                                    initialPage lgmod

                                LoginShowMessage _ li url ->
                                    let
                                        ( m, cmd ) =
                                            parseUrl url
                                                |> Maybe.andThen
                                                    (\s ->
                                                        case s of
                                                            Top ->
                                                                Nothing

                                                            _ ->
                                                                Just s
                                                    )
                                                |> Maybe.map
                                                    (routeState
                                                        lgmod
                                                    )
                                                |> Maybe.map (\( st, cm ) -> ( { model | state = st }, cm ))
                                                |> Maybe.withDefault (initialPage lgmod)
                                    in
                                    ( m, cmd )

                                _ ->
                                    ( displayMessageDialog model "logged in"
                                    , Cmd.none
                                    )

                        UI.LoggedOut ->
                            ( model, Cmd.none )

                        UI.ResetPasswordAck ->
                            let
                                nmod =
                                    { model
                                        | state =
                                            initLoginState model
                                    }
                            in
                            ( displayMessageDialog nmod "password reset attempted!  if you're a valid user, check your inbox for a reset email."
                            , Cmd.none
                            )

                        UI.SetPasswordAck ->
                            let
                                nmod =
                                    { model
                                        | state =
                                            initLoginState model
                                    }
                            in
                            ( displayMessageDialog nmod "password reset complete!"
                            , Cmd.none
                            )

                        UI.ChangedPassword ->
                            ( displayMessageDialog model "password changed"
                            , Cmd.none
                            )

                        UI.ChangedEmail ->
                            ( displayMessageDialog model <|
                                "email change confirmation sent!  check your inbox (or spam folder) for an email with title 'change "
                                    ++ model.appname
                                    ++ " email', and follow the enclosed link to change to the new address."
                            , Cmd.none
                            )

                        UI.UserExists ->
                            case state of
                                Login lmod ->
                                    ( { model | state = Login <| Login.userExists lmod }, Cmd.none )

                                _ ->
                                    ( unexpectedMessage model (UI.showServerResponse uiresponse)
                                    , Cmd.none
                                    )

                        UI.UnregisteredUser ->
                            case state of
                                Login lmod ->
                                    ( { model | state = Login <| Login.unregisteredUser lmod }, Cmd.none )

                                _ ->
                                    ( unexpectedMessage model (UI.showServerResponse uiresponse)
                                    , Cmd.none
                                    )

                        UI.NotLoggedIn ->
                            case state of
                                Login lmod ->
                                    ( { model | state = Login lmod }, Cmd.none )

                                _ ->
                                    ( { model | state = initLoginState model }, Cmd.none )

                        UI.InvalidUserOrPwd ->
                            case state of
                                Login lmod ->
                                    ( { model | state = Login <| Login.invalidUserOrPwd lmod }, Cmd.none )

                                _ ->
                                    ( unexpectedMessage { model | state = initLoginState model }
                                        (UI.showServerResponse uiresponse)
                                    , Cmd.none
                                    )

                        UI.Invite invite ->
                            ( { model | state = Invited (Invited.initialModel invite model.adminSettings "zknotes") }
                            , Cmd.none
                            )

        ( AdminReplyData ard, state ) ->
            case ard of
                Err e ->
                    ( displayMessageDialog model <| Util.httpErrorString e, Cmd.none )

                Ok airesponse ->
                    case airesponse of
                        AI.NotLoggedIn ->
                            case state of
                                Login lmod ->
                                    ( { model | state = Login lmod }, Cmd.none )

                                _ ->
                                    ( { model | state = initLoginState model }, Cmd.none )

                        AI.Users users ->
                            case stateLogin model.state of
                                Just login ->
                                    ( { model | state = UserListing (UserListing.init users) login }, Cmd.none )

                                Nothing ->
                                    ( displayMessageDialog model "not logged in", Cmd.none )

                        AI.UserDeleted id ->
                            ( displayMessageDialog model "user deleted!"
                            , sendAIMsg model.location AI.GetUsers
                            )

                        AI.UserUpdated ld ->
                            case model.state of
                                UserEdit ue login ->
                                    ( displayMessageDialog { model | state = UserEdit (UserEdit.onUserUpdated ue ld) login } "user updated"
                                    , Cmd.none
                                    )

                                _ ->
                                    ( model, Cmd.none )

                        AI.UserInvite ui ->
                            case stateLogin model.state of
                                Just login ->
                                    ( { model | state = UserInvite (UserInvite.init ui) login }
                                    , Cmd.none
                                    )

                                Nothing ->
                                    ( displayMessageDialog model "not logged in!"
                                    , Cmd.none
                                    )

                        AI.ServerError e ->
                            ( displayMessageDialog model <| e, Cmd.none )

        ( UserListingMsg umsg, UserListing umod login ) ->
            let
                ( numod, c ) =
                    UserListing.update umsg umod
            in
            case c of
                UserListing.Done ->
                    initialPage model

                UserListing.NewUser ->
                    -- let
                    --     ( sp, sr ) =
                    --         s
                    --             |> Maybe.withDefault
                    --                 ( SP.initModel
                    --                 , { notes = []
                    --                   , offset = 0
                    --                   , what = ""
                    --                   }
                    --                 )
                    -- in
                    -- ( { model
                    --     | state =
                    --         InviteUser
                    --             (InviteUser.init sp sr model.recentNotes [] login)
                    --             login
                    --   }
                    -- , Cmd.none
                    -- )
                    ( model, Cmd.none )

                UserListing.InviteUser ->
                    ( { model | state = UserListing numod login }
                    , sendAIMsg model.location (AI.GetInvite { email = Nothing, data = Nothing })
                    )

                UserListing.EditUser ld ->
                    ( { model | state = UserEdit (UserEdit.init ld) login }, Cmd.none )

                UserListing.None ->
                    ( { model | state = UserListing numod login }, Cmd.none )

        ( UserEditMsg umsg, UserEdit umod login ) ->
            let
                ( numod, c ) =
                    UserEdit.update umsg umod
            in
            case c of
                UserEdit.Done ->
                    ( model
                    , sendAIMsg model.location AI.GetUsers
                    )

                UserEdit.Delete id ->
                    ( model
                    , sendAIMsg model.location <| AI.DeleteUser (getUserIdVal id)
                    )

                UserEdit.Save ld ->
                    ( model
                    , sendAIMsg model.location <| AI.UpdateUser ld
                    )

                UserEdit.None ->
                    ( { model | state = UserEdit numod login }, Cmd.none )

        ( UserInviteMsg umsg, UserInvite umod login ) ->
            let
                ( numod, c ) =
                    UserInvite.update umsg umod
            in
            case c of
                UserInvite.Done ->
                    ( model
                    , sendAIMsg model.location AI.GetUsers
                    )

                UserInvite.None ->
                    ( { model | state = UserInvite numod login }, Cmd.none )

        ( TimeclonkReplyData urd, state ) ->
            case urd of
                Err e ->
                    ( displayMessageDialog model <| Util.httpErrorString e, Cmd.none )

                Ok uiresponse ->
                    case uiresponse of
                        TI.ServerError e ->
                            ( displayMessageDialog model <| e, Cmd.none )

                        TI.NotLoggedIn ->
                            ( { model | state = initLoginState model }, Cmd.none )

                        TI.InvalidUserOrPwd ->
                            ( { model | state = initLoginState model }, Cmd.none )

                        TI.ProjectList x ->
                            case stateLogin state of
                                Just login ->
                                    ( { model | state = ProjectListing (ProjectListing.init x) login }, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                        TI.ProjectEdit x ->
                            case stateLogin state of
                                Just login ->
                                    ( { model | state = ProjectEdit (ProjectEdit.initEdit x.project x.members) login }, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                        TI.ProjectTime x ->
                            case state of
                                ProjectTime st login ->
                                    ( { model | state = ProjectTime (ProjectTime.onProjectTime model.timezone login x st) login }, Cmd.none )

                                _ ->
                                    openProjectTime model "" x

                        TI.SavedProjectEdit x ->
                            case state of
                                ProjectEdit s l ->
                                    ( { model | state = ProjectEdit (ProjectEdit.onSavedProjectEdit x s) l }, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )

                        TI.AllUsers x ->
                            case state of
                                ProjectEdit s l ->
                                    let
                                        alms =
                                            x |> List.map (\m -> ( m.id, m )) |> TDict.insertList TR.emptyUmDict

                                        somems =
                                            -- should be TDict.diff, but ...
                                            --    https://github.com/bburdette/typed-collections/issues/3
                                            TDict.foldl
                                                (\k v t -> TDict.remove k t)
                                                alms
                                                s.members
                                    in
                                    ( { model
                                        | state =
                                            SelectUserDialog
                                                (SS.init
                                                    { choices = somems |> TDict.values |> List.map (\m -> ( m, m.name ))
                                                    , selected = Nothing
                                                    , search = ""
                                                    }
                                                    Common.buttonStyle
                                                    (E.map (always ()) (ProjectEdit.view l model.size s))
                                                )
                                                state
                                      }
                                    , Cmd.none
                                    )

                                _ ->
                                    ( model, Cmd.none )

        ( PublicReplyData urd, state ) ->
            case urd of
                Err e ->
                    ( displayMessageDialog model <| Util.httpErrorString e, Cmd.none )

                Ok uiresponse ->
                    case uiresponse of
                        PI.ServerError e ->
                            ( displayMessageDialog model <| e, Cmd.none )

                        PI.ProjectTime x ->
                            case state of
                                ProjectView st mblogin ->
                                    ( { model | state = ProjectView (ProjectView.onProjectTime model.timezone x st) mblogin }, Cmd.none )

                                _ ->
                                    ( { model | state = ProjectView (ProjectView.init model.timezone x model.pageincrement "") (stateLogin state) }, Cmd.none )

        ( DisplayMessageMsg bm, DisplayMessage bs prevstate ) ->
            case GD.update bm bs of
                GD.Dialog nmod ->
                    ( { model | state = DisplayMessage nmod prevstate }, Cmd.none )

                GD.Ok _ ->
                    case prevstate of
                        ShowMessage _ _ (Just ps) ->
                            ( { model | state = ps }, Cmd.none )

                        PubShowMessage _ (Just ps) ->
                            ( { model | state = ps }, Cmd.none )

                        _ ->
                            ( { model | state = prevstate }, Cmd.none )

                GD.Cancel ->
                    ( { model | state = prevstate }, Cmd.none )

        ( Noop, _ ) ->
            ( model, Cmd.none )

        ( ChangePasswordDialogMsg GD.Noop, _ ) ->
            ( model, Cmd.none )

        ( ChangeEmailDialogMsg GD.Noop, _ ) ->
            ( model, Cmd.none )

        ( SelectUserDialogMsg sdmsg, SelectUserDialog sdmod instate ) ->
            case GD.update sdmsg sdmod of
                GD.Dialog nmod ->
                    ( { model | state = SelectUserDialog nmod instate }, Cmd.none )

                GD.Ok return ->
                    case instate of
                        ProjectEdit pemod login ->
                            ( { model | state = ProjectEdit (ProjectEdit.addMember return Data.Member pemod) login }
                            , Cmd.none
                            )

                        ProjectTime ptmod login ->
                            ( { model | state = ProjectTime (ProjectTime.onMemberSelected return.id ptmod) login }
                            , Cmd.none
                            )

                        _ ->
                            ( { model | state = instate }, Cmd.none )

                GD.Cancel ->
                    ( { model | state = instate }, Cmd.none )

        ( SelectRoleDialogMsg sdmsg, SelectRoleDialog sdmod instate ) ->
            case GD.update sdmsg sdmod of
                GD.Dialog nmod ->
                    ( { model | state = SelectRoleDialog nmod instate }, Cmd.none )

                GD.Ok return ->
                    case instate of
                        ProjectEdit pemod login ->
                            ( { model | state = ProjectEdit (ProjectEdit.setRole return pemod) login }
                            , Cmd.none
                            )

                        _ ->
                            ( { model | state = instate }, Cmd.none )

                GD.Cancel ->
                    ( { model | state = instate }, Cmd.none )

        ( SelectUserDialogMsg GD.Noop, _ ) ->
            ( model, Cmd.none )

        ( SelectRoleDialogMsg GD.Noop, _ ) ->
            ( model, Cmd.none )

        ( DisplayMessageMsg GD.Noop, _ ) ->
            ( model, Cmd.none )

        ( ProjectListingMsg ms, ProjectListing st login ) ->
            let
                ( nm, cmd ) =
                    ProjectListing.update ms st login
            in
            case cmd of
                ProjectListing.Selected id ->
                    ( { model | state = ProjectListing nm login }
                    , sendTIMsg model.location <| TI.GetProjectEdit id
                    )

                ProjectListing.New ->
                    ( { model | state = ProjectEdit (ProjectEdit.initNew login) login }
                    , Cmd.none
                    )

                ProjectListing.Done ->
                    ( { model | state = ProjectListing nm login }, Cmd.none )

                ProjectListing.Settings ->
                    ( { model
                        | state =
                            UserSettings (UserSettings.init (Data.ldToOdLd login) model.fontsize model.saveonclonk model.pageincrement) login model.state
                      }
                    , Cmd.none
                    )

                ProjectListing.Admin ->
                    ( model
                    , sendAIMsg model.location AI.GetUsers
                    )

                ProjectListing.None ->
                    ( { model | state = ProjectListing nm login }, Cmd.none )

        ( ProjectEditMsg ms, ProjectEdit st login ) ->
            handleProjectEdit model (ProjectEdit.update ms st login) login

        ( WkMsg rkey, ProjectEdit ptm login ) ->
            case rkey of
                Ok key ->
                    handleProjectEdit model (ProjectEdit.onWkKeyPress key ptm login) login

                Err _ ->
                    ( model, Cmd.none )

        ( WkMsg rkey, ProjectTime ptm login ) ->
            case rkey of
                Ok key ->
                    handleProjectTime model (ProjectTime.onWkKeyPress key ptm login model.timezone) login

                Err _ ->
                    ( model, Cmd.none )

        ( ProjectTimeMsg ms, ProjectTime st login ) ->
            handleProjectTime model (ProjectTime.update ms st login model.timezone) login

        ( ProjectViewMsg ms, ProjectView st mblogin ) ->
            handleProjectView model (ProjectView.update ms st model.timezone) mblogin

        ( x, y ) ->
            ( unexpectedMsg model x
            , Cmd.none
            )


handleProjectEdit : Model -> ( ProjectEdit.Model, ProjectEdit.Command ) -> Data.LoginData -> ( Model, Cmd Msg )
handleProjectEdit model ( nm, cmd ) login =
    case cmd of
        ProjectEdit.Save s ->
            ( { model | state = ProjectEdit nm login }
            , sendTIMsg model.location <| TI.SaveProjectEdit s
            )

        ProjectEdit.New ->
            ( { model | state = ProjectEdit (ProjectEdit.initNew login) login }
            , Cmd.none
            )

        ProjectEdit.AddMember ->
            ( { model | state = ProjectEdit nm login }
            , sendTIMsg model.location <| TI.GetAllUsers
            )

        ProjectEdit.SelectRole id ->
            ( { model
                | state =
                    SelectRoleDialog
                        (SS.init
                            { choices =
                                [ Data.Member
                                , Data.Admin
                                , Data.Observer
                                ]
                                    |> List.map (\r -> ( ( id, r ), Data.showRole r ))
                            , selected = Nothing
                            , search = ""
                            }
                            Common.buttonStyle
                            (E.map (always ()) (ProjectEdit.view login model.size nm))
                        )
                        (ProjectEdit nm login)
              }
            , Cmd.none
            )

        ProjectEdit.Done ->
            ( { model | state = ProjectEdit nm login }
            , sendTIMsg model.location <| TI.GetProjectList login.userid
            )

        ProjectEdit.Settings ->
            ( { model
                | state =
                    UserSettings (UserSettings.init (Data.ldToOdLd login) model.fontsize model.saveonclonk model.pageincrement) login model.state
              }
            , Cmd.none
            )

        ProjectEdit.None ->
            ( { model | state = ProjectEdit nm login }, Cmd.none )


handleProjectTime : Model -> ( ProjectTime.Model, ProjectTime.Command ) -> Data.LoginData -> ( Model, Cmd Msg )
handleProjectTime model ( nm, cmd ) login =
    case cmd of
        ProjectTime.Save s ->
            ( { model | state = ProjectTime nm login }
            , sendTIMsg model.location <| TI.SaveProjectTime s
            )

        ProjectTime.Edit ->
            ( { model | state = ProjectEdit (ProjectEdit.initEdit nm.project nm.members) login }
            , Cmd.none
            )

        ProjectTime.GetTime tomsg ->
            ( { model | state = ProjectTime nm login }
            , Task.perform (Time.posixToMillis >> tomsg >> ProjectTimeMsg) Time.now
            )

        ProjectTime.Done ->
            ( { model | state = ProjectTime nm login }
            , sendTIMsg model.location <| TI.GetProjectList login.userid
            )

        ProjectTime.Settings ->
            ( { model
                | state =
                    UserSettings (UserSettings.init (Data.ldToOdLd login) model.fontsize model.saveonclonk model.pageincrement) login model.state
              }
            , Cmd.none
            )

        ProjectTime.GetCsv ->
            ( { model | state = ProjectTime nm login }
            , FS.file [ "text/csv" ] (FileLoaded (ProjectTimeMsg << ProjectTime.CsvString))
            )

        ProjectTime.SaveCsv filename csvstring ->
            ( { model | state = ProjectTime nm login }
            , FD.string filename "text/csv" csvstring
            )

        ProjectTime.ShowError e ->
            ( displayMessageDialog { model | state = ProjectTime nm login } e, Cmd.none )

        ProjectTime.SelectMember members ->
            ( { model
                | state =
                    SelectUserDialog
                        (SS.init
                            { choices = members |> List.map (\m -> ( m, m.name ))
                            , selected = Nothing
                            , search = ""
                            }
                            Common.buttonStyle
                            (E.map (always ()) (ProjectTime.view login model.size model.timezone nm))
                        )
                        (ProjectTime nm login)
              }
            , Cmd.none
            )

        ProjectTime.None ->
            ( { model | state = ProjectTime nm login }, Cmd.none )


handleProjectView : Model -> ( ProjectView.Model, ProjectView.Command ) -> Maybe Data.LoginData -> ( Model, Cmd Msg )
handleProjectView model ( nm, cmd ) mblogin =
    case cmd of
        ProjectView.Done ->
            case mblogin of
                Just login ->
                    ( { model | state = ProjectView nm mblogin }
                    , sendTIMsg model.location <| TI.GetProjectList login.userid
                    )

                Nothing ->
                    ( model, Cmd.none )

        ProjectView.Settings ->
            case mblogin of
                Just login ->
                    ( { model
                        | state =
                            UserSettings (UserSettings.init (Data.ldToOdLd login) model.fontsize model.saveonclonk model.pageincrement) login model.state
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ProjectView.SaveCsv filename csvstring ->
            ( { model | state = ProjectView nm mblogin }
            , FD.string filename "text/csv" csvstring
            )

        ProjectView.ShowError e ->
            ( displayMessageDialog { model | state = ProjectView nm mblogin } e, Cmd.none )

        ProjectView.None ->
            ( { model | state = ProjectView nm mblogin }, Cmd.none )


handleLogin : Model -> ( Login.Model, Login.Cmd ) -> ( Model, Cmd Msg )
handleLogin model ( lmod, lcmd ) =
    case lcmd of
        Login.None ->
            ( { model | state = Login lmod }, Cmd.none )

        Login.Register ->
            ( { model | state = Login lmod }
            , sendUIMsg model.location
                (UI.Register
                    { uid = lmod.userId
                    , pwd = lmod.password
                    , email = lmod.email
                    }
                )
            )

        Login.Login ->
            ( { model | state = Login lmod }
            , sendUIMsg model.location <|
                UI.Login
                    { uid = lmod.userId
                    , pwd = lmod.password
                    }
            )

        Login.Reset ->
            ( { model | state = Login lmod }
            , sendUIMsg model.location <|
                UI.ResetPassword
                    { uid = lmod.userId
                    }
            )


handleInvited : Model -> ( Invited.Model, Invited.Cmd ) -> ( Model, Cmd Msg )
handleInvited model ( lmod, lcmd ) =
    case lcmd of
        Invited.None ->
            ( { model | state = Invited lmod }, Cmd.none )

        Invited.RSVP ->
            ( { model | state = Invited lmod }
            , sendUIMsg model.location
                (UI.RSVP
                    { uid = lmod.userId
                    , pwd = lmod.password
                    , email = lmod.email
                    , invite = lmod.invite
                    }
                )
            )


preinit : Flags -> Url -> Browser.Navigation.Key -> ( PiModel, Cmd Msg )
preinit flags url key =
    ( PreInit
        { flags = flags
        , url = url
        , key = key
        , mbzone = Nothing
        , mbfontsize = Nothing
        , mbsaveonclonk = Nothing
        , mbpageincrement = Nothing
        }
    , Cmd.batch
        [ Task.perform Zone Time.here
        , LS.getLocalVal { for = "", name = "fontsize" }
        , LS.getLocalVal { for = "", name = "saveonclonk" }
        , LS.getLocalVal { for = "", name = "pageincrement" }
        ]
    )


initialPage : Model -> ( Model, Cmd Msg )
initialPage curmodel =
    (case stateLogin curmodel.state of
        Just login ->
            ( { curmodel
                | state = ShowMessage { message = "congrats, you are logged in!" } login Nothing
              }
            , sendTIMsg curmodel.location <| TI.GetProjectList login.userid
            )

        Nothing ->
            ( { curmodel | state = initLoginState curmodel }, Cmd.none )
    )
        |> (\( m, c ) ->
                ( m
                , Cmd.batch
                    [ Browser.Navigation.replaceUrl m.navkey
                        (routeUrl (stateRoute m.state).route)
                    , c
                    ]
                )
           )


init : Flags -> Url -> Browser.Navigation.Key -> Time.Zone -> Int -> Bool -> Int -> ( Model, Cmd Msg )
init flags url key zone fontsize saveonclonk pageincrement =
    let
        seed =
            initialSeed (flags.seed + 7)

        adminSettings =
            flags.adminsettings
                |> Maybe.andThen
                    (\v ->
                        JD.decodeValue OD.decodeAdminSettings v
                            |> Result.toMaybe
                    )
                |> Maybe.withDefault { openRegistration = False }

        imodel =
            { state =
                case flags.login of
                    Nothing ->
                        PubShowMessage { message = "loading..." } Nothing

                    Just v ->
                        case
                            JD.decodeValue OD.decodeLoginData v
                        of
                            Ok l ->
                                ShowMessage { message = "loading..." } (Data.odLdToLd l) Nothing

                            Err e ->
                                PubShowMessage { message = JD.errorToString e } Nothing
            , size = { width = flags.width, height = flags.height }
            , location = flags.location
            , appname = flags.appname
            , navkey = key
            , seed = seed
            , timezone = zone
            , savedRoute = { route = Top, save = False }
            , fontsize = fontsize
            , saveonclonk = saveonclonk
            , pageincrement = pageincrement
            , stylePalette = { defaultSpacing = 10 }
            , adminSettings = adminSettings
            }

        setkeys =
            skcommand <|
                WindowKeys.SetWindowKeys
                    [ { key = "s", ctrl = True, alt = False, shift = False, preventDefault = True }
                    , { key = "s", ctrl = True, alt = True, shift = False, preventDefault = True }
                    , { key = "e", ctrl = True, alt = True, shift = False, preventDefault = True }
                    , { key = "r", ctrl = True, alt = True, shift = False, preventDefault = True }
                    , { key = "v", ctrl = True, alt = True, shift = False, preventDefault = True }
                    , { key = "Enter", ctrl = False, alt = False, shift = False, preventDefault = False }
                    ]
    in
    parseUrl url
        |> Maybe.andThen
            (\s ->
                case s of
                    Top ->
                        Nothing

                    _ ->
                        Just s
            )
        |> Maybe.map
            (routeState
                imodel
            )
        |> Maybe.map
            (\( rs, rcmd ) ->
                ( { imodel
                    | state = rs
                  }
                , Cmd.batch [ rcmd, setkeys ]
                )
            )
        |> Maybe.withDefault
            (let
                ( m, c ) =
                    initialPage imodel
             in
             ( m
             , Cmd.batch
                [ c
                , setkeys
                , Browser.Navigation.replaceUrl key "/"
                ]
             )
            )


main : Platform.Program Flags PiModel Msg
main =
    Browser.application
        { init = preinit
        , view = piview
        , update = piupdate
        , subscriptions =
            \_ ->
                Sub.batch
                    [ receiveSelectedText SelectedText
                    , Browser.Events.onResize (\w h -> WindowSize { width = w, height = h })
                    , keyreceive
                    , LS.localVal ReceiveLocalVal
                    , Time.every 1000 ClockTick
                    ]
        , onUrlRequest = urlRequest
        , onUrlChange = UrlChanged
        }


port getSelectedText : List String -> Cmd msg


port receiveSelectedText : (JD.Value -> msg) -> Sub msg


port receiveKeyMsg : (JD.Value -> msg) -> Sub msg


keyreceive =
    receiveKeyMsg <| WindowKeys.receive WkMsg


port sendKeyCommand : JE.Value -> Cmd msg


skcommand =
    WindowKeys.send sendKeyCommand
