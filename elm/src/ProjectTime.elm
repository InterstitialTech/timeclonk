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
import Time
import Toop
import Util
import WindowKeys as WK


type Msg
    = DescriptionChanged String
    | SavePress
    | RevertPress
    | DonePress
    | EditPress
    | ClonkInPress
    | ClonkOutPress
    | ClonkInTime Int
    | ClonkOutTime Int
    | Noop


type alias EditTimeEntry =
    { id : Maybe Int
    , description : String
    , startdate : Int
    , enddate : Int
    }


type alias Model =
    { project : Data.Project
    , members : List Data.ProjectMember
    , description : String
    , timeentries : Dict Int EditTimeEntry
    , initialtimeentries : Dict Int EditTimeEntry
    }


type Command
    = Save Data.SaveProjectTime
    | Edit
    | Done
    | GetTime (Int -> Msg)
    | None


toSaveProjectTime : Model -> Data.SaveProjectTime
toSaveProjectTime model =
    { project = model.project.id
    , savetimeentries = []
    , deletetimeentries = []
    }


toEditTimeEntry : Data.TimeEntry -> EditTimeEntry
toEditTimeEntry te =
    { id = Just te.id
    , description = te.description
    , startdate = te.startdate
    , enddate = te.enddate
    }


toEteDict : List Data.TimeEntry -> Dict Int EditTimeEntry
toEteDict te =
    te
        |> List.map (toEditTimeEntry >> (\ete -> ( ete.startdate, ete )))
        |> Dict.fromList


onSavedProjectTime : List Data.TimeEntry -> Model -> Model
onSavedProjectTime te model =
    let
        ietes =
            toEteDict te
    in
    { model
        | timeentries = ietes
        , initialtimeentries = ietes
    }


isDirty : Model -> Bool
isDirty model =
    model.timeentries /= model.initialtimeentries


init : Data.ProjectTime -> Model
init pt =
    let
        ietes =
            toEteDict pt.timeentries
    in
    { project = pt.project
    , members = pt.members
    , description = ""
    , timeentries = ietes
    , initialtimeentries = ietes
    }


view : Data.LoginData -> Util.Size -> Time.Zone -> Model -> Element Msg
view ld size zone model =
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
            , E.row [] [ E.text "project name", E.text model.project.name ]
            , E.row [ E.spacing 8 ]
                [ EI.button Common.buttonStyle { onPress = Just DonePress, label = E.text "<-" }
                , EI.button Common.buttonStyle { onPress = Just EditPress, label = E.text "edit project" }
                , EI.button
                    (if isdirty then
                        Common.buttonStyle ++ [ EBk.color TC.darkYellow ]

                     else
                        Common.buttonStyle
                    )
                    { onPress = Just SavePress, label = E.text "save" }
                ]
            , E.table [ E.spacing 8 ]
                { data = model.timeentries |> Dict.values
                , columns =
                    [ { header = E.text "Task"
                      , width = E.shrink
                      , view = \te -> E.text te.description
                      }
                    , { header = E.text "Start"
                      , width = E.shrink
                      , view = \te -> E.text <| Util.showTime zone (Time.millisToPosix te.startdate)
                      }
                    , { header = E.text "End"
                      , width = E.shrink
                      , view = \te -> E.text <| Util.showTime zone (Time.millisToPosix te.enddate)
                      }
                    , { header = E.text "Duration"
                      , width = E.shrink
                      , view = \te -> E.text <| Util.showTime zone (Time.millisToPosix (te.enddate - te.startdate))
                      }
                    ]
                }
            , E.row [ E.width E.fill, E.spacing 8 ]
                [ EI.text [ E.width E.fill ]
                    { onChange = DescriptionChanged
                    , text = model.description
                    , placeholder = Nothing
                    , label = EI.labelLeft [] <| E.text "Current Task:"
                    }
                , EI.button Common.buttonStyle { onPress = Just ClonkInPress, label = E.text "Clonk In" }
                , EI.button Common.buttonStyle { onPress = Just ClonkOutPress, label = E.text "Clonk Out" }
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

        ClonkInPress ->
            ( model, GetTime ClonkInTime )

        ClonkOutPress ->
            ( model, GetTime ClonkOutTime )

        ClonkInTime time ->
            ( { model
                | timeentries =
                    Dict.insert time
                        { id = Nothing
                        , description = model.description
                        , startdate = time
                        , enddate = time
                        }
                        model.timeentries
              }
            , None
            )

        ClonkOutTime time ->
            ( { model
                | timeentries =
                    model.timeentries
                        |> Dict.values
                        |> List.reverse
                        >> List.head
                        |> Maybe.map (\t -> Dict.insert t.startdate { t | enddate = time } model.timeentries)
                        |> Maybe.withDefault model.timeentries
              }
            , None
            )

        DonePress ->
            ( model, Done )

        Noop ->
            ( model, None )
