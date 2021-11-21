port module Main exposing (main)

import Array
import Browser
import Browser.Events
import Browser.Navigation
import ChangeEmail as CE
import ChangePassword as CP
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
import File.Select as FS
import GenDialog as GD
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events as HE
import Http
import Json.Decode as JD
import Json.Encode as JE
import LocalStorage as LS
import Login
import ProjectEdit
import ProjectListing
import ProjectTime
import Random exposing (Seed, initialSeed)
import ResetPassword
import Route exposing (Route(..), parseUrl, routeTitle, routeUrl)
import SelectString as SS
import ShowMessage
import TangoColors as TC
import Task exposing (Task)
import Time
import UUID exposing (UUID)
import Url exposing (Url)
import Url.Builder as UB
import Url.Parser as UP exposing ((</>))
import UserInterface as UI
import UserSettings
import Util
import WindowKeys


type Msg
    = LoginMsg Login.Msg
    | UserSettingsMsg UserSettings.Msg
    | ShowMessageMsg ShowMessage.Msg
    | UserReplyData (Result Http.Error UI.ServerResponse)
    | LoadUrl String
    | InternalUrl Url
    | SelectedText JD.Value
    | UrlChanged Url
    | WindowSize Util.Size
    | DisplayMessageMsg (GD.Msg DisplayMessage.Msg)
    | SelectDialogMsg (GD.Msg (SS.Msg Data.ProjectMember))
    | ChangePasswordDialogMsg (GD.Msg CP.Msg)
    | ChangeEmailDialogMsg (GD.Msg CE.Msg)
    | ResetPasswordMsg ResetPassword.Msg
    | Zone Time.Zone
    | WkMsg (Result JD.Error WindowKeys.Key)
    | ReceiveLocalVal { for : String, name : String, value : Maybe String }
    | ProjectListingMsg ProjectListing.Msg
    | ProjectEditMsg ProjectEdit.Msg
    | ProjectTimeMsg ProjectTime.Msg
    | Noop


type State
    = Login Login.Model
    | UserSettings UserSettings.Model Data.LoginData State
    | ShowMessage ShowMessage.Model Data.LoginData (Maybe State)
    | PubShowMessage ShowMessage.Model (Maybe State)
    | LoginShowMessage ShowMessage.Model Data.LoginData Url
    | SelectDialog (SS.GDModel Data.ProjectMember) State
    | ChangePasswordDialog CP.GDModel State
    | ChangeEmailDialog CE.GDModel State
    | ResetPassword ResetPassword.Model
    | DisplayMessage DisplayMessage.GDModel State
    | Wait State (Model -> Msg -> ( Model, Cmd Msg ))
    | ProjectListing ProjectListing.Model Data.LoginData
    | ProjectEdit ProjectEdit.Model Data.LoginData
    | ProjectTime ProjectTime.Model Data.LoginData


type alias Flags =
    { seed : Int
    , location : String
    , useragent : String
    , appname : String
    , debugstring : String
    , width : Int
    , height : Int
    , login : Maybe Data.LoginData
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
    }


type alias PreInitModel =
    { flags : Flags
    , url : Url
    , key : Browser.Navigation.Key
    , mbzone : Maybe Time.Zone
    , mbfontsize : Maybe Int
    }


type PiModel
    = Ready Model
    | PreInit PreInitModel


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
            ( Login (Login.initialModel Nothing model.appname model.seed), Cmd.none )

        ResetPasswordR username key ->
            ( ResetPassword <| ResetPassword.initialModel username key model.appname, Cmd.none )

        SettingsR ->
            case stateLogin model.state of
                Just login ->
                    ( UserSettings (UserSettings.init login model.fontsize) login model.state, Cmd.none )

                Nothing ->
                    ( (displayMessageDialog { model | state = initLogin model.appname model.seed } "can't view user settings; you're not logged in!").state, Cmd.none )

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
            , sendUIMsg model.location <| UI.GetProjectEdit id
            )

        ProjectTimeR id ->
            ( (displayMessageDialog model "loading project").state
            , sendUIMsg model.location <| UI.GetProjectTime id
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

        _ ->
            { route = Top
            , save = False
            }


showMessage : Msg -> String
showMessage msg =
    case msg of
        LoginMsg _ ->
            "LoginMsg"

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

        Noop ->
            "Noop"

        WkMsg _ ->
            "WkMsg"

        ReceiveLocalVal _ ->
            "ReceiveLocalVal"

        SelectDialogMsg _ ->
            "SelectDialogMsg"

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

        ProjectEditMsg _ ->
            "ProjectEditMsg"

        ProjectTimeMsg _ ->
            "ProjectTimeMsg"


showState : State -> String
showState state =
    case state of
        Login _ ->
            "Login"

        UserSettings _ _ _ ->
            "UserSettings"

        DisplayMessage _ _ ->
            "DisplayMessage"

        ShowMessage _ _ _ ->
            "ShowMessage"

        PubShowMessage _ _ ->
            "PubShowMessage"

        LoginShowMessage _ _ _ ->
            "LoginShowMessage"

        Wait _ _ ->
            "Wait"

        SelectDialog _ _ ->
            "SelectDialog"

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
            E.map LoginMsg <| Login.view size lem

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

        -- E.map DisplayMessageMsg <| DisplayMessage.view em
        Wait innerState _ ->
            E.map (\_ -> Noop) (viewState size innerState model)

        SelectDialog _ _ ->
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


stateLogin : State -> Maybe Data.LoginData
stateLogin state =
    case state of
        Login _ ->
            Nothing

        UserSettings _ login _ ->
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

        SelectDialog _ instate ->
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

            SelectDialog sdm _ ->
                Html.map SelectDialogMsg <|
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

                                _ ->
                                    { imod | mbfontsize = Nothing }

                        _ ->
                            imod
            in
            case ( nmod.mbzone, nmod.mbfontsize ) of
                ( Just zone, Just fontsize ) ->
                    let
                        ( m, c ) =
                            init imod.flags imod.url imod.key zone fontsize
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
                    ( { model | state = prevstate }, Cmd.none )

                UserSettings.LogOut ->
                    ( { model | state = Login (Login.initialModel Nothing model.appname model.seed) }
                    , sendUIMsg model.location UI.Logout
                    )

                UserSettings.ChangePassword ->
                    ( { model
                        | state =
                            ChangePasswordDialog (CP.init login Common.buttonStyle (UserSettings.view numod |> E.map (always ())))
                                (UserSettings numod login prevstate)
                      }
                    , Cmd.none
                    )

                UserSettings.ChangeEmail ->
                    ( { model
                        | state =
                            ChangeEmailDialog (CE.init login Common.buttonStyle (UserSettings.view numod |> E.map (always ())))
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
                                                login
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
                                            Login <| Login.initialModel Nothing model.appname model.seed
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
                                            Login <| Login.initialModel Nothing model.appname model.seed
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
                                    ( { model | state = Login <| Login.initialModel Nothing model.appname model.seed }, Cmd.none )

                        UI.InvalidUserOrPwd ->
                            case state of
                                Login lmod ->
                                    ( { model | state = Login <| Login.invalidUserOrPwd lmod }, Cmd.none )

                                _ ->
                                    ( unexpectedMessage { model | state = Login (Login.initialModel Nothing model.appname model.seed) }
                                        (UI.showServerResponse uiresponse)
                                    , Cmd.none
                                    )

                        UI.ProjectList x ->
                            case stateLogin state of
                                Just login ->
                                    ( { model | state = ProjectListing (ProjectListing.init x) login }, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                        UI.ProjectEdit x ->
                            case stateLogin state of
                                Just login ->
                                    ( { model | state = ProjectEdit (ProjectEdit.initEdit x.project x.members) login }, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                        UI.ProjectTime x ->
                            case stateLogin state of
                                Just login ->
                                    ( { model | state = ProjectTime (ProjectTime.init login x) login }, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                        UI.SavedProjectEdit x ->
                            case state of
                                ProjectEdit s l ->
                                    ( { model | state = ProjectEdit (ProjectEdit.onSavedProjectEdit x s) l }, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )

                        UI.AllMembers x ->
                            case state of
                                ProjectEdit s l ->
                                    let
                                        alms =
                                            x |> List.map (\m -> ( m.id, m )) |> Dict.fromList

                                        somems =
                                            Dict.diff alms s.members
                                    in
                                    ( { model
                                        | state =
                                            SelectDialog
                                                (SS.init
                                                    { choices = somems |> Dict.values |> List.map (\m -> ( m, m.name ))
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

        ( SelectDialogMsg sdmsg, SelectDialog sdmod instate ) ->
            case GD.update sdmsg sdmod of
                GD.Dialog nmod ->
                    ( { model | state = SelectDialog nmod instate }, Cmd.none )

                GD.Ok return ->
                    case instate of
                        ProjectEdit pemod login ->
                            ( { model | state = ProjectEdit (ProjectEdit.addMember return pemod) login }
                            , Cmd.none
                            )

                        _ ->
                            ( { model | state = instate }, Cmd.none )

                GD.Cancel ->
                    ( { model | state = instate }, Cmd.none )

        ( SelectDialogMsg GD.Noop, _ ) ->
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
                    , sendUIMsg model.location <| UI.GetProjectEdit id
                    )

                ProjectListing.New ->
                    ( { model | state = ProjectEdit ProjectEdit.initNew login }
                    , Cmd.none
                    )

                ProjectListing.Done ->
                    ( { model | state = ProjectListing nm login }, Cmd.none )

                ProjectListing.Settings ->
                    ( { model
                        | state =
                            UserSettings (UserSettings.init login model.fontsize) login model.state
                      }
                    , Cmd.none
                    )

                ProjectListing.None ->
                    ( { model | state = ProjectListing nm login }, Cmd.none )

        ( ProjectEditMsg ms, ProjectEdit st login ) ->
            let
                ( nm, cmd ) =
                    ProjectEdit.update ms st login
            in
            case cmd of
                ProjectEdit.Save s ->
                    ( { model | state = ProjectEdit nm login }
                    , sendUIMsg model.location <| UI.SaveProjectEdit s
                    )

                ProjectEdit.New ->
                    ( { model | state = ProjectEdit ProjectEdit.initNew login }
                    , Cmd.none
                    )

                ProjectEdit.AddMember ->
                    ( { model | state = ProjectEdit nm login }
                    , sendUIMsg model.location <| UI.GetAllMembers
                    )

                ProjectEdit.Done ->
                    ( { model | state = ProjectEdit nm login }
                    , sendUIMsg model.location <| UI.GetProjectList login.userid
                    )

                ProjectEdit.Settings ->
                    ( { model
                        | state =
                            UserSettings (UserSettings.init login model.fontsize) login model.state
                      }
                    , Cmd.none
                    )

                ProjectEdit.None ->
                    ( { model | state = ProjectEdit nm login }, Cmd.none )

        ( ProjectTimeMsg ms, ProjectTime st login ) ->
            let
                ( nm, cmd ) =
                    ProjectTime.update ms st login
            in
            case cmd of
                ProjectTime.Save s ->
                    ( { model | state = ProjectTime nm login }
                    , sendUIMsg model.location <| UI.SaveProjectTime s
                    )

                ProjectTime.Edit ->
                    ( { model | state = ProjectEdit (ProjectEdit.initEdit st.project st.members) login }
                    , Cmd.none
                    )

                ProjectTime.GetTime tomsg ->
                    ( { model | state = ProjectTime nm login }
                    , Task.perform (Time.posixToMillis >> tomsg >> ProjectTimeMsg) Time.now
                    )

                ProjectTime.Done ->
                    ( { model | state = ProjectTime nm login }
                    , sendUIMsg model.location <| UI.GetProjectList login.userid
                    )

                ProjectTime.Settings ->
                    ( { model
                        | state =
                            UserSettings (UserSettings.init login model.fontsize) login model.state
                      }
                    , Cmd.none
                    )

                ProjectTime.None ->
                    ( { model | state = ProjectTime nm login }, Cmd.none )

        ( x, y ) ->
            ( unexpectedMsg model x
            , Cmd.none
            )


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


preinit : Flags -> Url -> Browser.Navigation.Key -> ( PiModel, Cmd Msg )
preinit flags url key =
    ( PreInit
        { flags = flags
        , url = url
        , key = key
        , mbzone = Nothing
        , mbfontsize = Nothing
        }
    , Cmd.batch
        [ Task.perform Zone Time.here
        , LS.getLocalVal { for = "", name = "fontsize" }
        ]
    )


initialPage : Model -> ( Model, Cmd Msg )
initialPage curmodel =
    (case stateLogin curmodel.state of
        Just login ->
            ( { curmodel
                | state = ShowMessage { message = "congrats, you are logged in!" } login Nothing
              }
            , sendUIMsg curmodel.location <| UI.GetProjectList login.userid
            )

        Nothing ->
            ( { curmodel | state = initLogin curmodel.appname curmodel.seed }, Cmd.none )
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


init : Flags -> Url -> Browser.Navigation.Key -> Time.Zone -> Int -> ( Model, Cmd Msg )
init flags url key zone fontsize =
    let
        seed =
            initialSeed (flags.seed + 7)

        imodel =
            { state =
                case flags.login of
                    Nothing ->
                        PubShowMessage { message = "loading..." } Nothing

                    Just l ->
                        ShowMessage { message = "loading..." } l Nothing
            , size = { width = flags.width, height = flags.height }
            , location = flags.location
            , appname = flags.appname
            , navkey = key
            , seed = seed
            , timezone = zone
            , savedRoute = { route = Top, save = False }
            , fontsize = fontsize
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


initLogin : String -> Seed -> State
initLogin appname seed =
    Login <| Login.initialModel Nothing appname seed


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
