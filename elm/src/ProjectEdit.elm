module ProjectEdit exposing (..)

import Common
import Data exposing (UserId)
import Dialog as D
import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Background as EBk
import Element.Border as EBd
import Element.Font as EF
import Element.Input as EI
import Element.Region
import SelectString
import TDict exposing (TDict)
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
    { id : Maybe Data.ProjectId
    , name : String
    , description : String
    , public : Bool
    , createdate : Maybe Int
    , changeddate : Maybe Int
    , members : TDict UserId Int Data.ProjectMember
    , initialProject : Maybe Data.Project
    , initialMembers : TDict UserId Int Data.ProjectMember
    }


type Command
    = Save Data.SaveProjectEdit
    | New
    | AddMember
    | Done
    | Settings
    | None


onWkKeyPress : WK.Key -> Model -> Data.LoginData -> ( Model, Command )
onWkKeyPress key model ld =
    case Toop.T4 key.key key.ctrl key.alt key.shift of
        Toop.T4 "s" True False False ->
            if isDirty model then
                update SavePress model ld

            else
                ( model, None )

        _ ->
            ( model, None )


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
        (TDict.diff model.members model.initialMembers
            |> TDict.values
            |> List.map (\m -> { id = m.id, delete = False })
        )
            ++ (TDict.diff model.initialMembers model.members
                    |> TDict.values
                    |> List.map (\m -> { id = m.id, delete = True })
               )
    }


emptyUmDict : TDict UserId Int Data.ProjectMember
emptyUmDict =
    TDict.empty Data.getUserIdVal Data.makeUserId


onSavedProjectEdit : Data.SavedProjectEdit -> Model -> Model
onSavedProjectEdit spe model =
    let
        mbrs =
            spe.members |> List.map (\m -> ( m.id, m )) |> TDict.insertList emptyUmDict
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
    { model | members = TDict.insert pm.id pm model.members }


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
    , members = emptyUmDict
    , initialProject = Nothing
    , initialMembers = emptyUmDict
    }


initEdit : Data.Project -> List Data.ProjectMember -> Model
initEdit proj members =
    let
        mbs =
            members
                |> List.map (\m -> ( m.id, m ))
                |> TDict.insertList emptyUmDict
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
            , E.row [ E.spacing 8 ] <|
                [ EI.button Common.buttonStyle { onPress = Just DonePress, label = E.text "<-" }
                , EI.button Common.buttonStyle { onPress = Just NewPress, label = E.text "new" }
                ]
                    ++ (if isdirty then
                            [ EI.button
                                (Common.buttonStyle ++ [ EBk.color TC.darkYellow ])
                                { onPress = Just SavePress, label = E.text "save" }
                            ]

                        else
                            []
                       )
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
                    :: (model.members |> TDict.values |> List.map (\m -> E.text m.name))
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
