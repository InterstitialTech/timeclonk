module InviteUser exposing (Command(..), Model, Msg(..), disabledLinkButtonStyle, emptyProjectDict, emptyProjectRoleDict, init, linkButtonStyle, update, view)

import Common
import Data exposing (ProjectId, Role(..), getProjectIdVal, makeProjectId)
import Dict exposing (Dict(..))
import Element as E exposing (Element)
import Element.Background as EBk
import Element.Border as EBd
import Element.Events as EE
import Element.Font as EF
import Element.Input as EI
import Element.Region
import Json.Encode as JE
import Orgauth.Data as OD
import TDict exposing (TDict(..))
import TangoColors
import TcCommon as TC
import Time exposing (Zone)
import Util


linkButtonStyle =
    Common.buttonStyle


disabledLinkButtonStyle =
    Common.disabledButtonStyle


type alias Model =
    { ld : Data.LoginData
    , projects : TDict ProjectId Int Data.ListProject
    , assignedProjects : TDict ProjectId Int Role
    , email : String
    }


type Msg
    = EmailChanged String
    | Add ProjectId Data.Role
    | Remove ProjectId
    | OkClick
    | CancelClick
    | Noop


type Command
    = None
    | GetInvite OD.GetInvite
    | Cancel


emptyProjectRoleDict : TDict ProjectId Int Role
emptyProjectRoleDict =
    TDict.empty getProjectIdVal makeProjectId


emptyProjectDict : TDict ProjectId Int Data.ListProject
emptyProjectDict =
    TDict.empty getProjectIdVal makeProjectId


init : List Data.ListProject -> Data.LoginData -> Model
init projects loginData =
    { ld = loginData
    , email = ""
    , projects =
        projects
            |> List.map (\p -> ( p.id, p ))
            |> TDict.insertList emptyProjectDict
    , assignedProjects = emptyProjectRoleDict
    }


view : TC.StylePalette -> Maybe Util.Size -> Model -> Element Msg
view stylePalette mbsize model =
    let
        sppad =
            [ E.paddingXY 0 5 ]

        spwidth =
            E.px
                400

        showAssigned =
            model.assignedProjects
                |> TDict.toList
                |> List.map
                    (\( pid, role ) ->
                        E.row [ E.width E.fill, E.spacing 8 ]
                            [ E.text
                                (TDict.get pid model.projects
                                    |> Maybe.map (\p -> p.name)
                                    |> Maybe.withDefault ""
                                )
                            , E.el [ E.alignRight ] <| E.text (Data.roleToString role)
                            , EI.button (E.alignRight :: Common.buttonStyle)
                                { onPress = Just <| Remove pid
                                , label = E.text "x"
                                }
                            ]
                    )

        showProjects =
            E.column [ E.spacing 8 ]
                (model.projects
                    |> TDict.toList
                    |> List.map
                        (\( pid, p ) ->
                            E.row [ E.width E.fill, E.spacing 8 ] <|
                                if p.role == Admin then
                                    [ E.text p.name
                                    , EI.button (E.alignRight :: Common.buttonStyle)
                                        { onPress = Just (Add pid Observer)
                                        , label = E.text "observer"
                                        }
                                    , EI.button (E.alignRight :: Common.buttonStyle)
                                        { onPress = Just (Add pid Member)
                                        , label = E.text "member"
                                        }
                                    , EI.button (E.alignRight :: Common.buttonStyle)
                                        { onPress = Just (Add pid Admin)
                                        , label = E.text "admin"
                                        }
                                    ]

                                else
                                    [ E.el [ EF.color TangoColors.grey ] <| E.text p.name
                                    ]
                        )
                )
    in
    E.column [ E.spacing 10 ]
        [ E.el [ EF.bold, E.centerX, EF.size 40 ] <| E.text "add new user"
        , E.row
            [ E.spacing 10
            ]
            [ E.column [ E.spacing 8, E.alignTop, E.width <| E.px 500 ] <|
                [ EI.text []
                    { onChange = EmailChanged
                    , text = model.email
                    , placeholder = Nothing
                    , label = EI.labelLeft [] (E.text "optional email")
                    }
                , E.row [ E.width E.fill, E.spacing 10 ]
                    [ EI.button
                        Common.buttonStyle
                        { onPress = Just OkClick, label = E.text "Ok" }
                    , EI.button
                        Common.buttonStyle
                        { onPress = Just CancelClick, label = E.text "Cancel" }
                    ]
                ]
                    ++ showAssigned
            , showProjects
            ]
        ]


update : Msg -> Model -> ( Model, Command )
update msg model =
    case msg of
        EmailChanged s ->
            ( { model | email = s }, None )

        Add pid role ->
            ( { model | assignedProjects = TDict.insert pid role model.assignedProjects }, None )

        Remove pid ->
            ( { model | assignedProjects = TDict.remove pid model.assignedProjects }, None )

        CancelClick ->
            ( model, Cancel )

        OkClick ->
            ( model
            , GetInvite
                { email =
                    if model.email /= "" then
                        Just model.email

                    else
                        Nothing
                , data =
                    Data.encodeUserInviteData
                        { projects =
                            model.assignedProjects
                                |> TDict.toList
                                |> List.map (\( pid, role ) -> { id = pid, role = role })
                        }
                        |> JE.encode 0
                        |> Just

                -- Data.encodeZkInviteData (List.map Data.elToSzl (Dict.values model.zklDict))
                --     |> JE.encode 2
                --     |> Just
                }
            )

        Noop ->
            ( model, None )
