module ProjectEdit exposing (..)

import Common
import Data
import Dialog as D
import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Background as EBk
import Element.Border as EBd
import Element.Font as EF
import Element.Input as EI
import Element.Region
import SelectString
import TangoColors as TC
import TcCommon as TC
import Toop
import Util
import WindowKeys as WK


type Msg
    = NameChanged String
    | DescriptionChanged String
    | SavePress
    | RevertPress
    | DonePress
    | SettingsPress
    | NewPress
    | AddMemberPress
    | Noop


type alias Model =
    { id : Maybe Int
    , name : String
    , description : String
    , public : Bool
    , createdate : Maybe Int
    , changeddate : Maybe Int
    , members : Dict Int Data.ProjectMember
    , initialProject : Maybe Data.Project
    , initialMembers : Dict Int Data.ProjectMember
    }


type Command
    = Save Data.SaveProjectEdit
    | New
    | AddMember
    | Done
    | Settings
    | None


toSaveProject : Model -> Data.SaveProject
toSaveProject model =
    { id = model.id
    , name = model.name
    , description = model.description
    , public = model.public
    }


toSaveProjectEdit : Model -> Data.SaveProjectEdit
toSaveProjectEdit model =
    { project = toSaveProject model
    , members =
        (Dict.diff model.members model.initialMembers
            |> Dict.values
            |> List.map (\m -> { id = m.id, delete = False })
        )
            ++ (Dict.diff model.initialMembers model.members
                    |> Dict.values
                    |> List.map (\m -> { id = m.id, delete = True })
               )
    }


onSavedProjectEdit : Data.SavedProjectEdit -> Model -> Model
onSavedProjectEdit spe model =
    let
        mbrs =
            spe.members |> List.map (\m -> ( m.id, m )) |> Dict.fromList
    in
    { model
        | id = Just spe.project.id
        , changeddate = Just spe.project.changeddate
        , createdate =
            model.createdate
                |> Maybe.withDefault spe.project.changeddate
                |> Just
        , members = mbrs
        , initialMembers = mbrs
        , initialProject = Just spe.project
    }


addMember : Data.ProjectMember -> Model -> Model
addMember pm model =
    { model | members = Dict.insert pm.id pm model.members }


isDirty : Model -> Bool
isDirty model =
    let
        projdirty =
            model.initialProject
                |> Maybe.map
                    (\ip ->
                        not
                            ((model.id == Just ip.id)
                                && (model.name == ip.name)
                                && (model.description == ip.description)
                                && (model.public == ip.public)
                                && (model.createdate == Just ip.createdate)
                                && (model.changeddate == Just ip.changeddate)
                            )
                    )
                |> Maybe.withDefault True

        membersdirty =
            model.members /= model.initialMembers
    in
    projdirty || membersdirty


initNew : Model
initNew =
    { id = Nothing
    , name = ""
    , description = ""
    , public = False
    , createdate = Nothing
    , changeddate = Nothing
    , members = Dict.empty
    , initialProject = Nothing
    , initialMembers = Dict.empty
    }


initEdit : Data.Project -> List Data.ProjectMember -> Model
initEdit proj members =
    let
        mbs =
            members
                |> List.map (\m -> ( m.id, m ))
                |> Dict.fromList
    in
    { id = Just proj.id
    , name = proj.name
    , description = proj.description
    , public = proj.public
    , createdate = Just proj.createdate
    , changeddate = Just proj.changeddate
    , members = mbs
    , initialProject = Just proj
    , initialMembers = mbs
    }


view : Data.LoginData -> Util.Size -> Model -> Element Msg
view ld size model =
    let
        maxwidth =
            700

        titlemaxconst =
            85

        isdirty =
            isDirty model
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
                    { onPress = Just SettingsPress, label = E.text "settings" }
                ]
            , E.row [ E.spacing 8 ]
                [ EI.button Common.buttonStyle { onPress = Just DonePress, label = E.text "<-" }
                , EI.button Common.buttonStyle { onPress = Just NewPress, label = E.text "new" }
                , EI.button
                    (if isdirty then
                        Common.buttonStyle ++ [ EBk.color TC.darkYellow ]

                     else
                        Common.buttonStyle
                    )
                    { onPress = Just SavePress, label = E.text "save" }
                ]
            , E.column
                [ E.padding 8
                , EBd.rounded 10
                , EBd.width 1
                , EBd.color TC.darkGrey
                , EBk.color TC.white
                , E.spacing 8
                ]
                [ EI.text
                    (if isdirty then
                        [ E.focused [ EBd.glow TC.darkYellow 3 ] ]

                     else
                        []
                    )
                    { onChange =
                        NameChanged
                    , text = model.name
                    , placeholder = Nothing
                    , label =
                        EI.labelLeft
                            []
                            (E.text "name")
                    }
                , EI.text
                    (if isdirty then
                        [ E.focused [ EBd.glow TC.darkYellow 3 ] ]

                     else
                        []
                    )
                    { onChange =
                        DescriptionChanged
                    , text = model.description
                    , placeholder = Nothing
                    , label =
                        EI.labelLeft
                            []
                            (E.text "description")
                    }
                ]
            , E.column
                [ E.padding 8
                , EBd.rounded 10
                , EBd.width 1
                , EBd.color TC.darkGrey
                , EBk.color TC.white
                , E.spacing 8
                ]
                (E.row [ E.spacing 10 ]
                    [ E.el [ EF.bold ] <| E.text "members"
                    , EI.button Common.buttonStyle { onPress = Just AddMemberPress, label = E.text "add" }
                    ]
                    :: (model.members |> Dict.values |> List.map (\m -> E.text m.name))
                )
            ]


update : Msg -> Model -> Data.LoginData -> ( Model, Command )
update msg model ld =
    case msg of
        NameChanged t ->
            ( { model | name = t }, None )

        DescriptionChanged t ->
            ( { model | description = t }, None )

        SavePress ->
            ( model, Save (toSaveProjectEdit model) )

        RevertPress ->
            ( model, None )

        NewPress ->
            ( model, New )

        DonePress ->
            ( model, Done )

        SettingsPress ->
            ( model, Settings )

        AddMemberPress ->
            ( model, AddMember )

        Noop ->
            ( model, None )
