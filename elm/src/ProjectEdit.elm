module ProjectEdit exposing (..)

import Common
import Data
import Dialog as D
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
    , members : List Data.ProjectMember
    , initialProject : Maybe Data.Project
    , initialMembers : List Data.ProjectMember
    , selectMember : Maybe (SelectString.GDModel Int)
    }


type Command
    = Save Data.SaveProject
    | New
    | GetMembers
    | Done
    | None


toSaveProject : Model -> Data.SaveProject
toSaveProject model =
    { id = model.id
    , name = model.name
    , description = model.description
    , public = model.public
    }


onSavedProject : Data.SavedProject -> Model -> Model
onSavedProject sp model =
    { model
        | id = Just sp.id
        , changeddate = Just sp.changeddate
        , createdate =
            model.createdate
                |> Maybe.withDefault sp.changeddate
                |> Just
    }


onAllMembers : List Data.ProjectMember -> Data.LoginData -> Util.Size -> Model -> Model
onAllMembers allmembers ld size model =
    { model
        | selectMember =
            Just
                (SelectString.init
                    { choices = List.map (\m -> ( m.id, m.name )) allmembers
                    , selected = Nothing
                    , search = ""
                    }
                    Common.buttonStyle
                    (E.map (always ()) (view ld size model))
                )
    }


isDirty : Model -> Bool
isDirty model =
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


initNew : Model
initNew =
    { id = Nothing
    , name = ""
    , description = ""
    , public = False
    , createdate = Nothing
    , changeddate = Nothing
    , members = []
    , initialProject = Nothing
    , initialMembers = []
    , selectMember = Nothing
    }


initEdit : Data.Project -> List Data.ProjectMember -> Model
initEdit proj members =
    { id = Just proj.id
    , name = proj.name
    , description = proj.description
    , public = proj.public
    , createdate = Just proj.createdate
    , changeddate = Just proj.changeddate
    , members = members
    , initialProject = Just proj
    , initialMembers = members
    , selectMember = Nothing
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
                    { onPress = Just DonePress, label = E.text "settings" }
                ]
            , E.row [ E.spacing 8 ]
                [ EI.button Common.buttonStyle { onPress = Just DonePress, label = E.text "<-" }
                , EI.button Common.buttonStyle { onPress = Just NewPress, label = E.text "new" }
                , EI.button Common.buttonStyle { onPress = Just SavePress, label = E.text "save" }
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
                    :: (model.members |> List.map (\m -> E.text m.name))
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
            ( model, Save (toSaveProject model) )

        RevertPress ->
            ( model, None )

        NewPress ->
            ( model, New )

        DonePress ->
            ( model, Done )

        AddMemberPress ->
            ( model, GetMembers )

        Noop ->
            ( model, None )
