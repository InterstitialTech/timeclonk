module ProjectTime exposing (..)

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
    = DescriptionChanged String
    | SavePress
    | RevertPress
    | DonePress
    | EditPress
    | Noop


type alias Model =
    { project : Data.Project
    , members : List Data.ProjectMember
    , description : String
    , timeentries : List Data.TimeEntry
    , initialtimeentries : List Data.TimeEntry
    }


type Command
    = Save Data.SaveProjectTime
    | Edit
    | Done
    | None


toSaveProjectTime : Model -> Data.SaveProjectTime
toSaveProjectTime model =
    { project = model.project.id
    , savetimeentries = []
    , deletetimeentries = []
    }


onSavedProjectTime : List Data.TimeEntry -> Model -> Model
onSavedProjectTime te model =
    { model
        | timeentries = te
        , initialtimeentries = te
    }


isDirty : Model -> Bool
isDirty model =
    model.timeentries /= model.initialtimeentries


init : Data.ProjectTime -> Model
init pt =
    { project = pt.project
    , members = pt.members
    , description = ""
    , timeentries = pt.timeentries
    , initialtimeentries = pt.timeentries
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
                , EI.button
                    (if isdirty then
                        Common.buttonStyle ++ [ EBk.color TC.darkYellow ]

                     else
                        Common.buttonStyle
                    )
                    { onPress = Just SavePress, label = E.text "save" }
                ]
            , E.table []
                { data = model.timeentries
                , columns =
                    [ { header = E.text "Task"
                      , width = E.shrink
                      , view = \te -> E.text te.description
                      }
                    ]
                }
            , E.column
                [ E.padding 8
                , EBd.rounded 10
                , EBd.width 1
                , EBd.color TC.darkGrey
                , EBk.color TC.white
                , E.spacing 8
                ]
                [ E.row [] [ E.text "project name", E.text model.project.name ]
                ]
            ]


update : Msg -> Model -> Data.LoginData -> ( Model, Command )
update msg model ld =
    case msg of
        DescriptionChanged t ->
            ( { model | description = t }, None )

        SavePress ->
            ( model, Save (toSaveProjectTime model) )

        RevertPress ->
            ( model, None )

        EditPress ->
            ( model, Edit )

        DonePress ->
            ( model, Done )

        Noop ->
            ( model, None )
