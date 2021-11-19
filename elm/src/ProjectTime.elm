module ProjectTime exposing (..)

import Calendar
import Common
import Data
import Dialog as D
import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Background as EBk
import Element.Border as EBd
import Element.Events as EE
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
    | EteDescriptionChanged Int String
    | EteStartChanged Int String
    | FocusDescriptionChanged String
    | FocusStartChanged Time.Zone String
    | FocusEndChanged Time.Zone String
    | SetViewMode ViewMode
    | OnRowClick Time.Zone Int
    | OnDistributionChanged String
    | ClearDistribution
    | CalcDistribution
    | Noop


type ViewMode
    = Clonk
    | Payment


type alias Model =
    { project : Data.Project
    , members : List Data.ProjectMember
    , description : String
    , timeentries : Dict Int EditTimeEntry
    , initialtimeentries : Dict Int EditTimeEntry
    , focusstart : String
    , focusend : String
    , focusdescription : String
    , focusrow : Maybe Int
    , distributionhours : String
    , distribution : Maybe (Dict Int Int)
    , viewmode : ViewMode
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
    , viewmode = Clonk
    , focusstart = ""
    , focusend = ""
    , focusdescription = ""
    , focusrow = Nothing
    , distributionhours = ""
    , distribution = Nothing
    }


viewModeBar : Model -> Element Msg
viewModeBar model =
    let
        vbt =
            \vm text ->
                EI.button
                    (if vm == model.viewmode then
                        Common.disabledButtonStyle

                     else
                        Common.buttonStyle
                    )
                    { onPress = Just (SetViewMode vm), label = E.text text }
    in
    E.row [ E.width E.fill, E.spacing 8 ]
        [ vbt Clonk "Clonks"
        , vbt Payment "Payments"
        ]


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
        <|
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
            , viewModeBar model

            -- , EI.text [ E.width E.fill ]
            --     { onChange = TestDateChanged zone
            --     , text = model.testdate
            --     , placeholder = Nothing
            --     , label = EI.labelHidden "test date"
            --     }
            ]
                ++ (case model.viewmode of
                        Clonk ->
                            clonkview ld size zone model

                        Payment ->
                            payview ld size zone model
                   )


clonkview : Data.LoginData -> Util.Size -> Time.Zone -> Model -> List (Element Msg)
clonkview ld size zone model =
    [ E.table [ E.spacing 8, E.width E.fill ]
        { data = model.timeentries |> Dict.values |> List.filter (\te -> te.user == ld.userid)
        , columns =
            [ { header = E.text "Task"
              , width = E.fill
              , view =
                    \te ->
                        let
                            row =
                                E.row [ EE.onClick <| OnRowClick zone te.startdate ] [ E.text te.description ]
                        in
                        if model.focusrow == Just te.startdate then
                            E.column []
                                [ row
                                , EI.text [ E.width E.fill ]
                                    { onChange = EteDescriptionChanged te.startdate
                                    , text = te.description
                                    , placeholder = Nothing
                                    , label = EI.labelHidden "task description"
                                    }
                                ]

                        else
                            row
              }
            , { header = E.text "Start"
              , width = E.fill
              , view =
                    \te ->
                        let
                            row =
                                E.row [ EE.onClick <| OnRowClick zone te.startdate ] [ E.text <| Util.showTime zone (Time.millisToPosix te.startdate) ]
                        in
                        if model.focusrow == Just te.startdate then
                            E.column []
                                [ row
                                , EI.text [ E.width E.fill ]
                                    { onChange = FocusStartChanged zone
                                    , text = model.focusstart
                                    , placeholder = Nothing
                                    , label = EI.labelHidden "task start date"
                                    }
                                ]

                        else
                            row
              }
            , { header = E.text "End"
              , width = E.fill
              , view =
                    \te ->
                        let
                            row =
                                E.row [ EE.onClick <| OnRowClick zone te.startdate ] [ E.text <| Util.showTime zone (Time.millisToPosix te.enddate) ]
                        in
                        if model.focusrow == Just te.startdate then
                            E.column []
                                [ row
                                , EI.text [ E.width E.fill ]
                                    { onChange = FocusEndChanged zone
                                    , text = model.focusend
                                    , placeholder = Nothing
                                    , label = EI.labelHidden "task end date"
                                    }
                                ]

                        else
                            row
              }
            , { header = E.text "Duration"
              , width = E.shrink
              , view = \te -> E.text <| R.round 2 (toFloat (te.enddate - te.startdate) / (1000.0 * 60.0 * 60.0))
              }
            ]
        }
    , E.row [ E.width E.fill, E.spacing 8 ]
        [ E.text "team hours: "
        , E.text <| (model.timeentries |> Dict.values |> TR.totalMillis |> TR.millisToHours)
        ]
    , E.row [ E.width E.fill, E.spacing 8 ]
        [ E.text "my hours: "
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


payview : Data.LoginData -> Util.Size -> Time.Zone -> Model -> List (Element Msg)
payview ld size zone model =
    let
        tmpd =
            TR.teamMillisPerDay (Dict.values model.timeentries)
    in
    [ E.table [ E.spacing 8, E.width E.fill ]
        { data = Dict.toList tmpd -- (date, Dict user millis)
        , columns =
            { header = E.text "date"
            , width = E.fill
            , view =
                \( date, ums ) ->
                    date
                        |> Time.millisToPosix
                        |> Calendar.fromPosix
                        |> (\cdate ->
                                E.text <|
                                    String.fromInt (Calendar.getYear cdate)
                                        ++ "/"
                                        ++ (cdate |> Calendar.getMonth |> Calendar.monthToInt |> String.fromInt)
                                        ++ "/"
                                        ++ String.fromInt
                                            (Calendar.getDay cdate)
                           )
            }
                :: (model.members
                        |> List.map
                            (\member ->
                                { header = E.text member.name
                                , width = E.fill
                                , view =
                                    \( date, ums ) ->
                                        case Dict.get member.id ums of
                                            Just millis ->
                                                if millis > 0 then
                                                    E.text <| R.round 2 (toFloat millis / (1000.0 * 60.0 * 60.0))

                                                else
                                                    E.none

                                            Nothing ->
                                                E.none
                                }
                            )
                   )
        }
    , E.row [ E.width E.fill, E.spacing 8 ]
        [ EI.text [ E.width E.fill ]
            { onChange = OnDistributionChanged
            , text = model.distributionhours
            , placeholder = Nothing
            , label = EI.labelLeft [] <| E.text "Calc Distribution:"
            }
        , EI.button Common.buttonStyle { onPress = Just CalcDistribution, label = E.text "calc" }
        , EI.button Common.buttonStyle { onPress = Just ClearDistribution, label = E.text "x" }
        ]
    , case model.distribution of
        Just dist ->
            let
                md =
                    model.members |> List.map (\m -> ( m.id, m )) |> Dict.fromList
            in
            E.table [ E.spacing 8, E.width E.fill ]
                { data = dist |> Dict.toList
                , columns =
                    [ { header = E.text "User"
                      , width = E.shrink
                      , view =
                            \( id, _ ) ->
                                case Dict.get id md of
                                    Just m ->
                                        E.text m.name

                                    Nothing ->
                                        E.none
                      }
                    , { header = E.text "Hours"
                      , width = E.shrink
                      , view =
                            \( _, millis ) ->
                                E.text (R.round 2 (toFloat millis / (60 * 60 * 1000)))
                      }
                    ]
                }

        Nothing ->
            E.none
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

        SetViewMode vm ->
            ( { model | viewmode = vm }, None )

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

        EteDescriptionChanged startdate text ->
            ( { model
                | timeentries =
                    case Dict.get startdate model.timeentries of
                        Just te ->
                            Dict.insert startdate { te | description = text } model.timeentries

                        Nothing ->
                            model.timeentries
              }
            , None
            )

        EteStartChanged startdate text ->
            ( { model
                | timeentries =
                    case Dict.get startdate model.timeentries of
                        Just te ->
                            Dict.insert startdate { te | description = text } model.timeentries

                        Nothing ->
                            model.timeentries
              }
            , None
            )

        OnRowClick zone i ->
            if model.focusrow == Just i then
                ( { model | focusrow = Nothing }, None )

            else
                case Dict.get i model.timeentries of
                    Just te ->
                        ( { model
                            | focusrow = Just i
                            , focusdescription = te.description
                            , focusstart = Util.showTime zone (Time.millisToPosix te.startdate)
                            , focusend = Util.showTime zone (Time.millisToPosix te.enddate)
                          }
                        , None
                        )

                    Nothing ->
                        ( model, None )

        FocusDescriptionChanged text ->
            case model.focusrow of
                Just startdate ->
                    ( { model
                        | timeentries =
                            case Dict.get startdate model.timeentries of
                                Just te ->
                                    Dict.insert startdate { te | description = text } model.timeentries

                                Nothing ->
                                    model.timeentries
                      }
                    , None
                    )

                Nothing ->
                    ( model, None )

        FocusStartChanged zone text ->
            case ( model.focusrow, Debug.log "parttimes" <| Util.parseTime zone text ) of
                ( Just startdate, Ok (Just time) ) ->
                    case Dict.get startdate model.timeentries of
                        Just te ->
                            let
                                newtime =
                                    Time.posixToMillis time
                            in
                            ( { model
                                | timeentries =
                                    Dict.insert newtime { te | startdate = newtime } model.timeentries
                                        |> Dict.remove startdate
                                , focusrow = Just <| newtime
                                , focusstart = text
                              }
                            , None
                            )

                        Nothing ->
                            ( { model | focusstart = text }, None )

                _ ->
                    ( { model | focusstart = text }, None )

        FocusEndChanged zone text ->
            case ( model.focusrow, Debug.log "parttimes" <| Util.parseTime zone text ) of
                ( Just startdate, Ok (Just time) ) ->
                    case Dict.get startdate model.timeentries of
                        Just te ->
                            let
                                newtime =
                                    Time.posixToMillis time
                            in
                            ( { model
                                | timeentries =
                                    Dict.insert startdate { te | enddate = newtime } model.timeentries
                                , focusend = text
                              }
                            , None
                            )

                        Nothing ->
                            ( { model | focusend = text }, None )

                _ ->
                    ( { model | focusend = text }, None )

        OnDistributionChanged text ->
            ( { model | distributionhours = text }, None )

        ClearDistribution ->
            ( { model | distributionhours = "", distribution = Nothing }, None )

        CalcDistribution ->
            case String.toFloat model.distributionhours of
                Just hours ->
                    let
                        tmpd =
                            TR.teamMillisPerDay (Dict.values model.timeentries)

                        distmillis =
                            round <|
                                hours
                                    * 60
                                    * 60
                                    * 1000

                        dist =
                            tmpd
                                |> Dict.toList
                                |> List.foldl
                                    (\( date, day ) ( sum, distamt ) ->
                                        let
                                            _ =
                                                Debug.log "( sum, distamt )" ( sum, distamt )

                                            daysum =
                                                Debug.log "daysum"
                                                    (day |> Dict.values |> List.foldl (+) 0)

                                            sumsum =
                                                Debug.log "sumsum"
                                                    (sum |> Dict.values |> List.foldl (+) 0)

                                            _ =
                                                Debug.log "(daysum + sumsum , distamt) " ( daysum + sumsum, distamt )
                                        in
                                        if daysum + sumsum > distamt then
                                            -- do last-day distrib.
                                            let
                                                _ =
                                                    Debug.log "lastday" sum

                                                distbelowavg daylist sumdict badistamt =
                                                    let
                                                        _ =
                                                            Debug.log "distbelowavg" ( daylist, sumdict, badistamt )

                                                        ddsize =
                                                            List.length daylist
                                                    in
                                                    if ddsize == 0 then
                                                        -- not supposed to run out of users in this scenario.
                                                        ( sumdict, badistamt )

                                                    else
                                                        let
                                                            avg =
                                                                Debug.log "avg" <|
                                                                    badistamt
                                                                        // ddsize

                                                            ( outer_dd, outer_sd, outer_da ) =
                                                                daylist
                                                                    |> List.foldl
                                                                        (\( user, millis ) ( dd, sd, da ) ->
                                                                            if millis < avg then
                                                                                ( dd
                                                                                , sd
                                                                                    |> Dict.get user
                                                                                    |> Maybe.map (\amt -> amt + millis)
                                                                                    |> Maybe.withDefault millis
                                                                                    |> (\ms -> Dict.insert user ms sd)
                                                                                , da - millis
                                                                                )

                                                                            else
                                                                                ( ( user, millis ) :: dd
                                                                                , sd
                                                                                , da
                                                                                )
                                                                        )
                                                                        ( [], sumdict, badistamt )
                                                        in
                                                        if List.length outer_dd == ddsize then
                                                            -- dist evenly to all users and exit.
                                                            ( List.foldl
                                                                (\user sd ->
                                                                    sd
                                                                        |> Dict.get user
                                                                        |> Maybe.map (\amt -> amt + avg)
                                                                        |> Maybe.withDefault avg
                                                                        |> (\ms -> Dict.insert user ms sd)
                                                                )
                                                                sumdict
                                                                (List.map Tuple.first outer_dd)
                                                            , 0
                                                            )

                                                        else if outer_da == 0 then
                                                            ( outer_sd, 0 )

                                                        else
                                                            distbelowavg outer_dd outer_sd outer_da
                                            in
                                            distbelowavg (Dict.toList day) sum distamt

                                        else
                                            Debug.log "fullday: "
                                                ( day
                                                    |> Dict.toList
                                                    |> List.foldl
                                                        (\( user, millis ) newsum ->
                                                            case Dict.get user newsum of
                                                                Just oldsum ->
                                                                    Dict.insert user (millis + oldsum) newsum

                                                                Nothing ->
                                                                    Dict.insert user millis newsum
                                                        )
                                                        sum
                                                , distamt - daysum
                                                )
                                    )
                                    ( Dict.empty, distmillis )
                    in
                    ( { model | distribution = Just (Tuple.first dist) }, None )

                Nothing ->
                    ( model, None )

        DonePress ->
            ( model, Done )

        Noop ->
            ( model, None )
