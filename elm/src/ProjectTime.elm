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
import Round as R
import SelectString
import TangoColors as TC
import TcCommon as TC
import Time
import TimeReporting as TR exposing (EditTimeEntry)
import Toop
import Util
import WindowKeys as WK


type Msg
    = DescriptionChanged String
    | SavePress
    | RevertPress
    | DonePress
    | EditPress
    | SettingsPress
    | ClonkInPress
    | ClonkOutPress
    | ClonkInTime Int
    | ClonkOutTime Int
    | Noop


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
    | Settings
    | None


toSaveProjectTime : Model -> Data.SaveProjectTime
toSaveProjectTime model =
    { project = model.project.id
    , savetimeentries =
        model.timeentries
            |> Dict.values
            |> List.foldl
                (\te saves ->
                    case te.id |> Maybe.andThen (\id -> Dict.get id model.initialtimeentries) of
                        Just ite ->
                            if te /= ite then
                                te :: saves

                            else
                                saves

                        Nothing ->
                            te :: saves
                )
                []
            |> List.map (toSaveTimeEntry model)
    , deletetimeentries = Dict.diff model.initialtimeentries model.timeentries |> Dict.keys
    }


toEditTimeEntry : Data.TimeEntry -> EditTimeEntry
toEditTimeEntry te =
    { id = Just te.id
    , user = te.user
    , description = te.description
    , startdate = te.startdate
    , enddate = te.enddate
    }


toSaveTimeEntry : Model -> EditTimeEntry -> Data.SaveTimeEntry
toSaveTimeEntry model ete =
    { id = ete.id
    , project = model.project.id
    , user = ete.user
    , description = ete.description
    , startdate = ete.startdate
    , enddate = ete.enddate
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
                    { onPress = Just SettingsPress, label = E.text "settings" }
                ]
            , E.row [ E.spacing 8 ] [ E.text "project:", E.el [ EF.bold ] <| E.text model.project.name ]
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
            , E.table [ E.spacing 8, E.width E.fill ]
                { data = model.timeentries |> Dict.values |> List.filter (\te -> te.user == ld.userid)
                , columns =
                    [ { header = E.text "Task"
                      , width = E.fill
                      , view = \te -> E.text te.description
                      }
                    , { header = E.text "Start"
                      , width = E.fill
                      , view = \te -> E.text <| Util.showTime zone (Time.millisToPosix te.startdate)
                      }
                    , { header = E.text "End"
                      , width = E.fill
                      , view = \te -> E.text <| Util.showTime zone (Time.millisToPosix te.enddate)
                      }
                    , { header = E.text "Duration"
                      , width = E.fill
                      , view = \te -> E.text <| R.round 2 (toFloat (te.enddate - te.startdate) / (1000.0 * 60.0 * 60.0))
                      }

                    -- , { header = E.text "Daily"
                    --   , width = E.fill
                    --   , view = \te -> E.text <| R.round 2 (toFloat (te.enddate - te.startdate) / (1000.0 * 60.0 * 60.0))
                    --   }
                    -- , { header = E.text "Weekly"
                    --   , width = E.fill
                    --   , view = \te -> E.text <| R.round 2 (toFloat (te.enddate - te.startdate) / (1000.0 * 60.0 * 60.0))
                    --   }
                    ]
                }
            , E.row [ E.width E.fill, E.spacing 8 ]
                [ E.text "team hours: "
                , E.text <| (model.timeentries |> Dict.values |> TR.totalMillis |> TR.millisToHours)
                , E.text "my hours: "
                , E.text <|
                    (model.timeentries
                        |> Dict.values
                        |> List.filter (\te -> te.user == ld.userid)
                        |> TR.totalMillis
                        |> TR.millisToHours
                    )
                ]
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

        SettingsPress ->
            ( model, Settings )

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
                        , user = ld.userid
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
                        |> List.filter (.user >> (==) ld.userid)
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
