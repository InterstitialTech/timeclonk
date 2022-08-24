module InviteUser exposing (Command(..), Model, Msg(..), NavChoice(..), SearchOrRecent(..), disabledLinkButtonStyle, emptyProjectDict, emptyProjectSet, init, linkButtonStyle, update, view)

import Common
import Data exposing (ProjectId, getProjectIdVal, makeProjectId)
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
import TSet exposing (TSet(..))
import TcCommon as TC
import Time exposing (Zone)
import Util


linkButtonStyle =
    Common.buttonStyle


disabledLinkButtonStyle =
    Common.disabledButtonStyle


type NavChoice
    = NcSearch
    | NcRecent


type SearchOrRecent
    = SearchView
    | RecentView


type alias Model =
    { ld : Data.LoginData
    , projects : TDict ProjectId Int String
    , assignedProjects : TSet ProjectId Int
    , email : String
    }


type Msg
    = EmailChanged String
    | OkClick
    | CancelClick
    | Noop


type Command
    = None
    | GetInvite OD.GetInvite
    | Cancel


emptyProjectSet : TSet ProjectId Int
emptyProjectSet =
    TSet.empty getProjectIdVal makeProjectId


emptyProjectDict : TDict ProjectId Int String
emptyProjectDict =
    TDict.empty getProjectIdVal makeProjectId


init : List Data.ListProject -> Data.LoginData -> Model
init projects loginData =
    { ld = loginData
    , email = ""
    , projects =
        projects
            |> List.map (\p -> ( p.id, p.name ))
            |> TDict.insertList emptyProjectDict
    , assignedProjects = emptyProjectSet
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
                |> TSet.toList
                |> List.map
                    (\pid ->
                        E.text
                            (TDict.get pid model.projects
                                |> Maybe.withDefault ""
                            )
                    )

        showProjects =
            E.column []
                (model.projects
                    |> TDict.toList
                    |> List.map
                        (\( pid, name ) ->
                            E.text name
                        )
                )
    in
    E.row
        [ -- E.width (mbsize |> Maybe.map .width |> Maybe.withDefault 500 |> E.px)
          -- , E.height E.fill
          -- , E.height E.shrink
          E.spacing 10
        ]
        [ E.column [] <|
            [ EI.text []
                { onChange = EmailChanged
                , text = model.email
                , placeholder = Nothing
                , label = EI.labelLeft [] (E.text "email")
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


update : Msg -> Model -> ( Model, Command )
update msg model =
    case msg of
        EmailChanged s ->
            ( { model | email = s }, None )

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
                , data = Nothing

                -- Data.encodeZkInviteData (List.map Data.elToSzl (Dict.values model.zklDict))
                --     |> JE.encode 2
                --     |> Just
                }
            )

        Noop ->
            ( model, None )
