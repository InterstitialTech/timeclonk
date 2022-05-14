module ProjectView exposing (..)

import Calendar
import Common
import Csv
import Data exposing (UserId)
import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Background as EBk
import Element.Border as EBd
import Element.Events as EE
import Element.Font as EF
import Element.Input as EI
import Paginator as P
import Round as R
import Set
import TDict exposing (TDict)
import TSet exposing (TSet)
import TangoColors as TC
import TcCommon as TC
import Time
import TimeReporting as TR exposing (EditAllocation, EditPayEntry, EditTimeEntry, csvToEditAllocations, csvToEditTimeEntries, eteToCsv, millisAsHours)
import TimeTotaler exposing (TTotaler, getTes, getTotes, mapTimeentry, mkTToteler, setTes)
import Toop
import Util
import WindowKeys as WK


type Msg
    = DonePress
    | SettingsPress
    | ExportAll
    | SetViewMode ViewMode
    | OnDistributionChanged String
    | OnDistCurrencyChanged String
    | ClearDistribution
    | CalcDistribution
    | TeamForward
    | TeamBack Int
    | TeamToStart
    | TeamToEnd Int
    | PeForward
    | PeBack Int
    | PeToStart
    | PeToEnd Int
    | AForward
    | ABack Int
    | AToStart
    | AToEnd Int
    | DForward
    | DBack Int
    | DToStart
    | DToEnd Int
    | Noop


type ViewMode
    = Team
    | Payments
    | Allocations
    | Distributions


type FocusColumn
    = Description
    | Start
    | End
    | Duration
    | PaymentDate
    | PaymentUser
    | PaymentAmount


type alias Model =
    { project : Data.Project
    , members : List Data.ProjectMember
    , membernames : Dict Int String

    -- , description : String
    -- , timeentries : TTotaler
    -- , tepaginator : P.Model Msg
    , teamentries : TTotaler
    , teampaginator : P.Model Msg
    , payentries : Dict Int EditPayEntry
    , pepaginator : P.Model Msg
    , allocations : Dict Int EditAllocation
    , apaginator : P.Model Msg

    -- , focusstart : String
    -- , focusend : String
    -- , focusduration : String
    -- , focusdescription : String
    -- , focus : Maybe ( Int, FocusColumn )
    -- , focuspay : String
    -- , focuspaydate : String
    , distributionhours : String
    , distributioncurrency : String
    , distribution : Maybe (TDict UserId Int String)
    , dpaginator : P.Model Msg
    , viewmode : ViewMode
    }


type Command
    = Done
      -- | GetTime (Int -> Msg)
      -- | GetCsv
    | SaveCsv String String
    | Settings
    | ShowError String
    | None


headerStyle : List (E.Attribute msg)
headerStyle =
    [ EF.bold ]


showViewMode : ViewMode -> String
showViewMode mode =
    case mode of
        Team ->
            "team"

        Payments ->
            "payments"

        Allocations ->
            "allocations"

        Distributions ->
            "distributions"


readViewMode : String -> Maybe ViewMode
readViewMode str =
    case String.toLower str of
        "team" ->
            Just Team

        "payments" ->
            Just Payments

        "allocations" ->
            Just Allocations

        "distributions" ->
            Just Distributions

        _ ->
            Nothing


emptyTimeEntryIdSet : TSet Data.TimeEntryId Int
emptyTimeEntryIdSet =
    TSet.empty Data.getTimeEntryIdVal Data.makeTimeEntryId


emptyPayEntryIdSet : TSet Data.PayEntryId Int
emptyPayEntryIdSet =
    TSet.empty Data.getPayEntryIdVal Data.makePayEntryId


emptyAllocationIdSet : TSet Data.AllocationId Int
emptyAllocationIdSet =
    TSet.empty Data.getAllocationIdVal Data.makeAllocationId


toEditTimeEntry : Data.TimeEntry -> EditTimeEntry
toEditTimeEntry te =
    { id = Just te.id
    , user = te.user
    , description = te.description
    , startdate = te.startdate
    , enddate = te.enddate
    , ignore = te.ignore
    , checked = False
    }


toEditPayEntry : Data.PayEntry -> EditPayEntry
toEditPayEntry te =
    { id = Just te.id
    , user = te.user
    , description = te.description
    , paymentdate = te.paymentdate
    , duration = te.duration
    , checked = False
    }


toEditAllocation : Data.Allocation -> EditAllocation
toEditAllocation e =
    { id = Just e.id
    , description = e.description
    , allocationdate = e.allocationdate
    , duration = e.duration
    , checked = False
    }


toEteDict : List Data.TimeEntry -> Dict Int EditTimeEntry
toEteDict te =
    te
        |> List.map (toEditTimeEntry >> (\ete -> ( ete.startdate, ete )))
        |> Dict.fromList


toEpeDict : List Data.PayEntry -> Dict Int EditPayEntry
toEpeDict pe =
    pe
        |> List.map (toEditPayEntry >> (\epe -> ( epe.paymentdate, epe )))
        |> Dict.fromList


toEaDict : List Data.Allocation -> Dict Int EditAllocation
toEaDict a =
    a
        |> List.map (toEditAllocation >> (\ea -> ( ea.allocationdate, ea )))
        |> Dict.fromList



{-

   onSavedProjectTime : List Data.TimeEntry -> Model -> Model
   onSavedProjectTime te model =
       let
           ietes =
               toEteDict te
       in
       { model
           | timeentries = setTes model.timeentries ietes
           , initialtimeentries = ietes
       }


   isDirty : Model -> Bool
   isDirty model =
       (getTes model.timeentries |> Dict.map (\_ te -> { te | checked = False }))
           /= model.initialtimeentries
           || (model.payentries |> Dict.map (\_ pe -> { pe | checked = False }))
           /= model.initialpayentries
           || (model.allocations |> Dict.map (\_ e -> { e | checked = False }))
           /= model.initialallocations

-}


init : Time.Zone -> Data.ProjectTime -> Int -> String -> Model
init zone pt pageincrement mode =
    let
        ietes =
            toEteDict pt.timeentries

        iepes =
            toEpeDict pt.payentries

        ieas =
            toEaDict pt.allocations
    in
    { project = pt.project
    , members = pt.members
    , membernames = pt.members |> List.map (\m -> ( Data.getUserIdVal m.id, m.name )) |> Dict.fromList

    -- , description = description
    -- , timeentries = mkTToteler ietes (\te -> te.user == ld.userid) zone
    , teamentries = mkTToteler ietes (always True) zone
    , teampaginator = P.init TeamForward TeamBack TeamToStart TeamToEnd P.End pageincrement
    , payentries = iepes
    , pepaginator = P.init PeForward PeBack PeToStart PeToEnd P.End pageincrement
    , allocations = ieas
    , apaginator = P.init AForward ABack AToStart AToEnd P.End pageincrement
    , viewmode = readViewMode mode |> Maybe.withDefault Team

    -- , focusstart = ""
    -- , focusend = ""
    -- , focusduration = ""
    -- , focusdescription = ""
    -- , focus = Nothing
    -- , focuspay = ""
    -- , focuspaydate = ""
    , distributionhours = ""
    , distributioncurrency = ""
    , distribution = Nothing
    , dpaginator = P.init DForward DBack DToStart DToEnd P.End pageincrement

    -- , allocdescription = ""
    -- , allochours = ""
    -- , alloccurrency = ""
    -- , paymenthours = ""
    -- , paymentcurrency = ""
    -- , paymentuser = Nothing
    }


setPageIncrement : Int -> Model -> Model
setPageIncrement pageincrement model =
    let
        tp =
            model.teampaginator

        pp =
            model.pepaginator

        ap =
            model.apaginator

        dp =
            model.dpaginator
    in
    { model
        | teampaginator = { tp | pageincrement = pageincrement }
        , pepaginator = { pp | pageincrement = pageincrement }
        , apaginator = { ap | pageincrement = pageincrement }
        , dpaginator = { dp | pageincrement = pageincrement }
    }


onProjectTime : Time.Zone -> Data.ProjectTime -> Model -> Model
onProjectTime zone pt model =
    let
        nm =
            init zone pt model.teampaginator.pageincrement (showViewMode model.viewmode)
    in
    { nm
        | teampaginator = model.teampaginator
        , pepaginator = model.pepaginator
        , apaginator = model.apaginator
        , dpaginator = model.dpaginator
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
    E.row [ E.width E.fill, E.spacing TC.defaultSpacing, E.paddingXY 0 8 ]
        [ vbt Team "team"
        , vbt Payments "payments"
        , vbt Allocations "allocations"
        , vbt Distributions "distributions"
        ]


view : Util.Size -> Time.Zone -> Model -> Element Msg
view size zone model =
    let
        maxwidth =
            700

        titlemaxconst =
            85
    in
    E.el
        [ E.width E.fill
        , EBk.color TC.lightGrey
        ]
    <|
        E.column
            [ E.spacing TC.defaultSpacing
            , E.padding 8
            , E.width (E.maximum maxwidth E.fill)
            , E.centerX
            , EBk.color TC.lightGrey
            ]
        <|
            [ E.row [ E.spacing TC.defaultSpacing, E.width E.fill ]
                [ EI.button
                    (E.alignRight :: Common.buttonStyle)
                    { onPress = Just SettingsPress, label = E.text "settings" }
                ]
            , E.row [ E.spacing TC.defaultSpacing ] [ E.text "project:", E.el [ EF.bold ] <| E.text model.project.name ]
            , E.row [ E.spacing TC.defaultSpacing ] <|
                [ EI.button Common.buttonStyle { onPress = Just DonePress, label = E.text "<-" }

                -- , EI.button Common.buttonStyle { onPress = Just EditPress, label = E.text "edit project" }
                , EI.button Common.buttonStyle { onPress = Just ExportAll, label = E.text "export" }
                ]
            , viewModeBar model
            ]
                ++ (case model.viewmode of
                        Team ->
                            teamview size zone model

                        Payments ->
                            payview size zone model

                        Allocations ->
                            allocationview size zone model

                        Distributions ->
                            distributionview size zone model
                   )


cellEditStyle =
    [ E.spacing TC.defaultSpacing, EBk.color TC.darkGrey, E.padding TC.defaultSpacing ]


dateTimeWidth =
    200


teamview : Util.Size -> Time.Zone -> Model -> List (Element Msg)
teamview size zone model =
    let
        ttotes =
            getTotes model.teamentries

        paytotes =
            model.payentries |> Dict.values |> TR.payTotes

        teampay =
            paytotes
                |> TDict.values
                |> List.foldl (+) 0
                |> TR.millisToHours

        teamalloc =
            model.allocations
                |> Dict.values
                |> List.foldl (\e t -> t + e.duration) 0
                |> TR.millisToHours

        igfont =
            \te ->
                if te.ignore then
                    EF.strike

                else
                    EF.regular
    in
    [ P.view ttotes.mtecount model.teampaginator
    , E.table [ E.spacing TC.defaultSpacing, E.width E.fill ]
        { data =
            P.filter model.teampaginator ttotes.mytimeentries
        , columns =
            [ { header = E.el headerStyle <| E.text "task"
              , width = E.fill
              , view =
                    \te ->
                        E.row
                            [ igfont te
                            , E.width E.fill
                            ]
                            [ E.text te.description ]
              }
            , { header = E.el headerStyle <| E.text "member"
              , width = E.fill
              , view =
                    \te ->
                        E.row
                            [ igfont te
                            , E.width E.fill
                            ]
                            [ E.text
                                (te.user
                                    |> Data.getUserIdVal
                                    |> (\i -> Dict.get i model.membernames)
                                    |> Maybe.withDefault ""
                                )
                            ]
              }
            , { header = E.el headerStyle <| E.text "start"
              , width = E.fill
              , view =
                    \te ->
                        E.row
                            [ igfont te
                            , E.width E.fill
                            ]
                            [ E.text <| Util.showDateTime zone (Time.millisToPosix te.startdate) ]
              }
            , { header = E.el headerStyle <| E.text "end"
              , width = E.fill
              , view =
                    \te ->
                        let
                            endtext =
                                \enddate ->
                                    if
                                        Util.sameDay zone
                                            (Time.millisToPosix te.startdate)
                                            (Time.millisToPosix enddate)
                                    then
                                        Util.showTime zone (Time.millisToPosix enddate)

                                    else
                                        Util.showDateTime zone (Time.millisToPosix enddate)
                        in
                        E.row
                            [ igfont te
                            , E.width E.fill
                            ]
                            [ E.text <| endtext te.enddate
                            ]
              }
            , { header = E.el headerStyle <| E.text "duration"
              , width = E.shrink
              , view =
                    \te ->
                        E.row
                            [ igfont te
                            , E.width E.fill
                            ]
                            [ E.text <| millisAsHours (te.enddate - te.startdate) ]
              }
            , { header = E.el headerStyle <| E.text "daily"
              , width = E.shrink
              , view =
                    \te ->
                        if Set.member te.startdate ttotes.lastofdays then
                            let
                                today =
                                    te.startdate
                                        |> Time.millisToPosix
                                        |> TR.toDate zone
                                        |> Maybe.map Calendar.toMillis
                            in
                            case today |> Maybe.andThen (\t -> Dict.get t ttotes.daytotes) of
                                Just millis ->
                                    E.text (millisAsHours millis)

                                Nothing ->
                                    E.none

                        else
                            E.none
              }
            , { header = E.el headerStyle <| E.text "weekly"
              , width = E.shrink
              , view =
                    \te ->
                        if Set.member te.startdate ttotes.lastofweeks then
                            let
                                today =
                                    te.startdate
                                        |> Time.millisToPosix
                                        |> TR.toDate zone
                                        |> Maybe.map TR.toSunday
                                        |> Maybe.map Calendar.toMillis
                            in
                            case today |> Maybe.andThen (\t -> Dict.get t ttotes.weektotes) of
                                Just millis ->
                                    E.text (millisAsHours millis)

                                Nothing ->
                                    E.none

                        else
                            E.none
              }
            ]
        }
    , E.table [ E.spacing TC.defaultSpacing, E.width E.fill ]
        { data =
            [ ( "team unpaid hours: "
              , R.round 2 <| ttotes.teamhours - teampay
              )
            , ( "team allocated hours: "
              , R.round 2 <| teamalloc - ttotes.teamhours
              )
            ]
        , columns =
            [ { header = E.none
              , width = E.shrink
              , view =
                    \( title, entry ) -> E.el headerStyle <| E.text title
              }
            , { header = E.none
              , width = E.shrink
              , view =
                    \( title, entry ) -> E.text entry
              }
            ]
        }
    ]


type Entry
    = TimeDay (TDict UserId Int Int)
    | PayEntry EditPayEntry
    | Allocation EditAllocation


distributionview : Util.Size -> Time.Zone -> Model -> List (Element Msg)
distributionview size zone model =
    let
        paytotes =
            model.payentries |> Dict.values |> TR.payTotes

        timetotes =
            getTes model.teamentries |> Dict.values |> TR.timeTotes

        unpaidtotes =
            timetotes
                |> TDict.foldl
                    (\k v up ->
                        case TDict.get k paytotes of
                            Just p ->
                                TDict.insert k (v - p) up

                            Nothing ->
                                TDict.insert k v up
                    )
                    TR.emptyUserTimeDict

        alloctote =
            model.allocations
                |> Dict.values
                |> List.foldl (\e t -> t + e.duration) 0

        tmpd =
            TR.teamMillisPerDay zone (Dict.values (getTes model.teamentries))

        anychecked =
            Dict.foldl (\_ pe c -> c || pe.checked) False model.payentries

        data =
            Dict.union (Dict.map (\i v -> TimeDay v) tmpd) <|
                Dict.union
                    (Dict.map (\i v -> PayEntry v) model.payentries)
                    (Dict.map (\i v -> Allocation v) model.allocations)
    in
    [ P.view (Dict.size data) model.dpaginator
    , E.table [ E.spacing TC.defaultSpacing, E.width E.fill ]
        { data =
            data
                |> Dict.toList
                |> P.filter model.dpaginator
        , columns =
            { header = E.el headerStyle <| E.text "date"
            , width = E.fill
            , view =
                \( date, entry ) ->
                    case entry of
                        TimeDay td ->
                            date
                                |> Time.millisToPosix
                                |> Calendar.fromPosix
                                |> (\cdate ->
                                        E.row []
                                            [ E.text <|
                                                String.fromInt (Calendar.getYear cdate)
                                                    ++ "/"
                                                    ++ (cdate
                                                            |> Calendar.getMonth
                                                            |> Calendar.monthToInt
                                                            |> String.fromInt
                                                       )
                                                    ++ "/"
                                                    ++ String.fromInt
                                                        (Calendar.getDay cdate)
                                            ]
                                   )

                        PayEntry pe ->
                            date
                                |> Time.millisToPosix
                                |> Calendar.fromPosix
                                |> (\cdate ->
                                        E.row
                                            [ EF.bold
                                            ]
                                            [ E.text <|
                                                String.fromInt (Calendar.getYear cdate)
                                                    ++ "/"
                                                    ++ (cdate |> Calendar.getMonth |> Calendar.monthToInt |> String.fromInt)
                                                    ++ "/"
                                                    ++ String.fromInt
                                                        (Calendar.getDay cdate)
                                            ]
                                   )

                        Allocation a ->
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
                                { header = E.el headerStyle <| E.text member.name
                                , width = E.fill
                                , view =
                                    \( date, e ) ->
                                        case e of
                                            Allocation _ ->
                                                E.none

                                            TimeDay ums ->
                                                case TDict.get member.id ums of
                                                    Just millis ->
                                                        if millis > 0 then
                                                            E.row []
                                                                [ E.text <| millisAsHours millis
                                                                ]

                                                        else
                                                            E.none

                                                    Nothing ->
                                                        E.none

                                            PayEntry epe ->
                                                if epe.user == member.id then
                                                    let
                                                        s =
                                                            millisAsHours epe.duration

                                                        p =
                                                            E.el [ EF.bold ] <| E.text <| s ++ " pmt"
                                                    in
                                                    E.row []
                                                        [ p
                                                        ]

                                                else
                                                    E.none
                                }
                            )
                   )
                ++ [ { header = E.el headerStyle <| E.text "team"
                     , width = E.fill
                     , view =
                        \( date, e ) ->
                            case e of
                                Allocation _ ->
                                    E.none

                                TimeDay ums ->
                                    ums
                                        |> TDict.values
                                        |> List.foldl (+) 0
                                        |> E.text
                                        << millisAsHours

                                PayEntry epe ->
                                    E.none
                     }
                   , { header = E.el [] <| E.el headerStyle <| E.text "allocations"
                     , width = E.fill
                     , view =
                        \( date, e ) ->
                            case e of
                                Allocation a ->
                                    let
                                        s =
                                            millisAsHours a.duration
                                    in
                                    E.el [] <| E.text <| s

                                _ ->
                                    E.none
                     }
                   ]
        }
    , E.table [ E.paddingXY 0 10, E.spacing TC.defaultSpacing, E.width E.fill ]
        { data =
            [ ( "hours worked", timetotes )
            , ( "hours paid", paytotes )
            , ( "hours unpaid", unpaidtotes )
            ]
        , columns =
            -- dummy checkboxes for alignment.  alpha 0 hides them.
            { header =
                EI.checkbox [ E.width E.shrink, E.alpha 0.0 ]
                    { onChange = \_ -> Noop
                    , icon = TC.checkboxIcon
                    , checked = False
                    , label = EI.labelHidden "alignment checkbox"
                    }
            , width = E.shrink
            , view =
                \_ ->
                    EI.checkbox [ E.width E.shrink, E.alpha 0.0 ]
                        { onChange = \_ -> Noop
                        , icon = TC.checkboxIcon
                        , checked = False
                        , label = EI.labelHidden "alignment checkbox"
                        }
            }
                :: { header = E.el headerStyle <| E.text "totals"
                   , width = E.fill
                   , view =
                        \( title, _ ) ->
                            E.el headerStyle <| E.text title
                   }
                :: (model.members
                        |> List.map
                            (\member ->
                                { header = E.el headerStyle <| E.text member.name
                                , width = E.fill
                                , view =
                                    \( _, totes ) ->
                                        TDict.get member.id totes
                                            |> Maybe.map (\t -> E.text <| millisAsHours t)
                                            |> Maybe.withDefault E.none
                                }
                            )
                   )
                ++ [ { header = E.column [] [ E.el headerStyle <| E.text "team" ]
                     , width = E.fill
                     , view =
                        \( _, tote ) ->
                            tote
                                |> TDict.values
                                |> List.foldl (+) 0
                                >> millisAsHours
                                >> E.text
                     }
                   , { header = E.column [] [ E.el headerStyle <| E.text "allocation", E.el headerStyle <| E.text "- team" ]
                     , width = E.fill
                     , view =
                        \( _, tote ) ->
                            tote
                                |> TDict.values
                                |> List.foldl (+) 0
                                |> (-) alloctote
                                >> millisAsHours
                                >> E.text
                     }
                   ]
        }
    , E.row [ E.width E.fill, E.spacing TC.defaultSpacing ]
        [ E.text "Calc Distribution:"
        , EI.text [ E.width E.fill ]
            { onChange = OnDistributionChanged
            , text = model.distributionhours
            , placeholder = Nothing
            , label = EI.labelRight [] (E.text "hours")
            }
        , EI.text [ E.width E.fill ]
            { onChange = OnDistCurrencyChanged
            , text = model.distributioncurrency
            , placeholder = Nothing
            , label = EI.labelRight [] (model.project.currency |> Maybe.withDefault "" |> E.text)
            }
        , EI.button Common.buttonStyle { onPress = Just CalcDistribution, label = E.text "calc" }
        , EI.button Common.buttonStyle { onPress = Just ClearDistribution, label = E.text "x" }
        ]
    , case model.distribution of
        Just dist ->
            let
                md =
                    model.members |> List.map (\m -> ( m.id, Data.projectMemberToUser m )) |> TDict.insertList TR.emptyUmDict
            in
            E.table [ E.spacing TC.defaultSpacing, E.width E.fill ]
                { data = dist |> TDict.toList
                , columns =
                    [ { header = E.el headerStyle <| E.text "User"
                      , width = E.shrink
                      , view =
                            \( id, _ ) ->
                                case TDict.get id md of
                                    Just m ->
                                        E.el [ E.centerY ] <| E.text m.name

                                    Nothing ->
                                        E.none
                      }
                    , { header = E.el headerStyle <| E.text "Hours"
                      , width = E.shrink
                      , view =
                            \( user, hours ) ->
                                E.text hours
                      }
                    , { header = E.el headerStyle <| E.text (model.project.currency |> Maybe.withDefault "")
                      , width = E.shrink
                      , view =
                            \( user, hours ) ->
                                hours
                                    |> String.toFloat
                                    |> Maybe.andThen
                                        (\h ->
                                            model.project.rate
                                                |> Maybe.map
                                                    (\r ->
                                                        h
                                                            * r
                                                            |> String.fromFloat
                                                    )
                                        )
                                    |> Maybe.withDefault ""
                                    |> E.text
                                    |> E.el [ E.centerY ]
                      }
                    ]
                }

        Nothing ->
            E.none
    ]


allocationview : Util.Size -> Time.Zone -> Model -> List (Element Msg)
allocationview size zone model =
    let
        paytote =
            model.payentries
                |> Dict.values
                |> List.foldl (\e t -> t + e.duration) 0

        timetote =
            getTotes model.teamentries
                |> .teammillis

        alloctote =
            model.allocations
                |> Dict.values
                |> List.foldl (\e t -> t + e.duration) 0
    in
    [ P.view (Dict.size model.allocations) model.apaginator
    , E.table [ E.spacing TC.defaultSpacing, E.width E.fill ]
        { data =
            Dict.toList model.allocations
                |> P.filter model.apaginator
        , columns =
            { header = E.el headerStyle <| E.text "date"
            , width = E.fill
            , view =
                \( date, a ) ->
                    date
                        |> Time.millisToPosix
                        |> Calendar.fromPosix
                        |> (\cdate ->
                                E.row
                                    []
                                    [ E.text <|
                                        String.fromInt (Calendar.getYear cdate)
                                            ++ "/"
                                            ++ (cdate |> Calendar.getMonth |> Calendar.monthToInt |> String.fromInt)
                                            ++ "/"
                                            ++ String.fromInt
                                                (Calendar.getDay cdate)
                                    ]
                           )
            }
                :: { header = E.el headerStyle <| E.text "description"
                   , width = E.fill
                   , view =
                        \( date, a ) ->
                            E.row []
                                [ E.text a.description
                                ]
                   }
                :: { header = E.el headerStyle <| E.text "allocation"
                   , width = E.fill
                   , view =
                        \( date, a ) ->
                            let
                                s =
                                    millisAsHours a.duration

                                p =
                                    E.el [] <| E.text <| s
                            in
                            E.row []
                                [ p
                                ]
                   }
                :: []
        }
    , E.table [ E.paddingXY 0 10, E.spacing TC.defaultSpacing, E.width E.fill ]
        { data =
            [ ( "hours worked", timetote )
            , ( "hours paid", paytote )
            , ( "hours unpaid", timetote - paytote )
            , ( "hours allocated", alloctote )
            , ( "hours allocated remaining", alloctote - timetote )
            ]
        , columns =
            -- dummy checkboxes for alignment.  alpha 0 hides them.
            { header =
                EI.checkbox [ E.width E.shrink, E.alpha 0.0 ]
                    { onChange = \_ -> Noop
                    , icon = TC.checkboxIcon
                    , checked = False
                    , label = EI.labelHidden "alignment checkbox"
                    }
            , width = E.shrink
            , view =
                \_ ->
                    EI.checkbox [ E.width E.shrink, E.alpha 0.0 ]
                        { onChange = \_ -> Noop
                        , icon = TC.checkboxIcon
                        , checked = False
                        , label = EI.labelHidden "alignment checkbox"
                        }
            }
                :: { header = E.el [ EF.bold ] <| E.el headerStyle <| E.text "totals"
                   , width = E.fill
                   , view =
                        \( title, _ ) ->
                            E.el headerStyle <| E.text title
                   }
                :: { header = E.none
                   , width = E.fill
                   , view =
                        \( _, tote ) -> E.text <| millisAsHours tote
                   }
                :: []
        }
    ]


payview : Util.Size -> Time.Zone -> Model -> List (Element Msg)
payview size zone model =
    let
        paytote =
            model.payentries
                |> Dict.values
                |> List.foldl (\e t -> t + e.duration) 0

        timetote =
            getTotes model.teamentries
                |> .teammillis

        alloctote =
            model.allocations
                |> Dict.values
                |> List.foldl (\e t -> t + e.duration) 0
    in
    [ P.view (Dict.size model.payentries) model.pepaginator
    , E.table [ E.spacing TC.defaultSpacing, E.width E.fill ]
        { data =
            Dict.toList model.payentries
                |> P.filter model.pepaginator
        , columns =
            { header = E.el headerStyle <| E.text "date"
            , width = E.fill
            , view =
                \( date, a ) ->
                    date
                        |> Time.millisToPosix
                        |> Calendar.fromPosix
                        |> (\cdate ->
                                E.row
                                    []
                                    [ E.text <|
                                        String.fromInt (Calendar.getYear cdate)
                                            ++ "/"
                                            ++ (cdate |> Calendar.getMonth |> Calendar.monthToInt |> String.fromInt)
                                            ++ "/"
                                            ++ String.fromInt
                                                (Calendar.getDay cdate)
                                    ]
                           )
            }
                :: { header = E.el headerStyle <| E.text "payee"
                   , width = E.fill
                   , view =
                        \( date, a ) ->
                            E.row []
                                [ E.text (a.user |> Data.getUserIdVal |> (\i -> Dict.get i model.membernames |> Maybe.withDefault ""))
                                ]
                   }
                :: { header = E.el headerStyle <| E.text "payment"
                   , width = E.fill
                   , view =
                        \( date, a ) ->
                            let
                                s =
                                    millisAsHours a.duration

                                p =
                                    E.el [] <| E.text <| s
                            in
                            E.row []
                                [ p
                                ]
                   }
                :: []
        }
    , E.table [ E.paddingXY 0 10, E.spacing TC.defaultSpacing, E.width E.fill ]
        { data =
            [ ( "hours worked", timetote )
            , ( "hours paid", paytote )
            , ( "hours unpaid", timetote - paytote )
            , ( "hours allocated", alloctote )
            , ( "hours allocated remaining", alloctote - timetote )
            ]
        , columns =
            -- dummy checkboxes for alignment.  alpha 0 hides them.
            { header =
                EI.checkbox [ E.width E.shrink, E.alpha 0.0 ]
                    { onChange = \_ -> Noop
                    , icon = TC.checkboxIcon
                    , checked = False
                    , label = EI.labelHidden "alignment checkbox"
                    }
            , width = E.shrink
            , view =
                \_ ->
                    EI.checkbox [ E.width E.shrink, E.alpha 0.0 ]
                        { onChange = \_ -> Noop
                        , icon = TC.checkboxIcon
                        , checked = False
                        , label = EI.labelHidden "alignment checkbox"
                        }
            }
                :: { header = E.el [ EF.bold ] <| E.el headerStyle <| E.text "totals"
                   , width = E.fill
                   , view =
                        \( title, _ ) ->
                            E.el headerStyle <| E.text title
                   }
                :: { header = E.none
                   , width = E.fill
                   , view =
                        \( _, tote ) -> E.text <| millisAsHours tote
                   }
                :: []
        }
    ]


update : Msg -> Model -> Time.Zone -> ( Model, Command )
update msg model zone =
    case msg of
        SettingsPress ->
            ( model, Settings )

        SetViewMode vm ->
            ( { model | viewmode = vm }, None )

        OnDistributionChanged text ->
            ( { model
                | distributionhours = text
                , distributioncurrency =
                    text
                        |> String.toFloat
                        |> Maybe.andThen
                            (\h ->
                                model.project.rate |> Maybe.map (\r -> r * h |> String.fromFloat)
                            )
                        |> Maybe.withDefault ""
              }
            , None
            )

        OnDistCurrencyChanged text ->
            ( { model
                | distributioncurrency = text
                , distributionhours =
                    text
                        |> String.toFloat
                        |> Maybe.andThen
                            (\c ->
                                model.project.rate |> Maybe.map (\r -> c / r |> String.fromFloat)
                            )
                        |> Maybe.withDefault ""
              }
            , None
            )

        ClearDistribution ->
            ( { model | distributionhours = "", distributioncurrency = "", distribution = Nothing }, None )

        CalcDistribution ->
            case String.toFloat model.distributionhours of
                Just hours ->
                    let
                        -- total millis per day for each member.
                        utmpd =
                            TR.teamMillisPerDay zone (Dict.values <| getTes model.teamentries)

                        -- total pay so far for each member.
                        paytotes =
                            TR.payTotes (Dict.values model.payentries)

                        meh : ( Dict Int (TDict.TDict UserId Int Int), TDict.TDict UserId Int Int )
                        meh =
                            utmpd
                                |> Dict.toList
                                |> List.foldl
                                    -- while ptotes is > 0, remove time from utime and ptotes.
                                    (\( date, utime ) ( outtime, ptotes ) ->
                                        let
                                            ( upday, newtotes ) =
                                                utime
                                                    |> TDict.toList
                                                    |> List.foldl
                                                        -- for each day subtract user time from the payment totals.
                                                        (\( user, time ) ( utim, ptots ) ->
                                                            case TDict.get user ptots of
                                                                Just ptime ->
                                                                    if ptime - time < 0 then
                                                                        ( TDict.insert user (time - ptime) utim
                                                                        , TDict.insert user 0 ptots
                                                                        )

                                                                    else
                                                                        ( TDict.remove user utim
                                                                        , TDict.insert user (ptime - time) ptots
                                                                        )

                                                                Nothing ->
                                                                    ( utim, ptots )
                                                        )
                                                        ( utime, ptotes )
                                        in
                                        ( ( date, upday ) :: outtime, newtotes )
                                    )
                                    ( [], paytotes )
                                |> (\( t, tots ) ->
                                        ( t
                                            |> List.filter
                                                (\( i, d ) ->
                                                    not <| TDict.isEmpty d
                                                )
                                            |> Dict.fromList
                                        , tots
                                        )
                                   )

                        ( tmpd, fintotes ) =
                            meh

                        -- total millseconds to distribute this time.
                        distmillis =
                            round <|
                                hours
                                    * 60
                                    * 60
                                    * 1000

                        -- add to pay totals for each user until we run out of pay.
                        -- on the last day split the remainder between users with a 'below average' algo
                        dist =
                            tmpd
                                |> Dict.toList
                                |> List.foldl
                                    (\( date, day ) ( sum, distamt ) ->
                                        let
                                            daysum =
                                                day |> TDict.values |> List.foldl (+) 0

                                            sumsum =
                                                sum |> TDict.values |> List.foldl (+) 0
                                        in
                                        if daysum + sumsum > distamt then
                                            -- do last-day distrib.
                                            let
                                                -- 'below average' distribution.  the shortest time periods get paid first.
                                                -- once all users are 'above average' then the money is split evenly
                                                -- between them.
                                                -- for example, if we have two users who wored 3 and 5 hours, and we want to
                                                -- distribute 2 hours between them, then each would get 1 hour of pay.
                                                distbelowavg daylist sumdict badistamt =
                                                    let
                                                        dl_len =
                                                            List.length daylist
                                                    in
                                                    if dl_len == 0 then
                                                        -- not supposed to run out of users in this scenario.
                                                        ( sumdict, badistamt )

                                                    else
                                                        let
                                                            avg =
                                                                badistamt // dl_len

                                                            ( outer_dd, outer_sd, outer_da ) =
                                                                daylist
                                                                    |> List.foldl
                                                                        (\( user, millis ) ( dd, sd, da ) ->
                                                                            if millis < avg then
                                                                                ( dd
                                                                                , sd
                                                                                    |> TDict.get user
                                                                                    |> Maybe.map (\amt -> amt + millis)
                                                                                    |> Maybe.withDefault millis
                                                                                    |> (\ms -> TDict.insert user ms sd)
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
                                                        if List.length outer_dd == dl_len then
                                                            -- dist evenly to all users and exit.
                                                            ( List.foldl
                                                                (\user sd ->
                                                                    sd
                                                                        |> TDict.get user
                                                                        |> Maybe.map (\amt -> amt + avg)
                                                                        |> Maybe.withDefault avg
                                                                        |> (\ms -> TDict.insert user ms sd)
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
                                            distbelowavg (TDict.toList day) sum distamt

                                        else
                                            ( day
                                                |> TDict.toList
                                                |> List.foldl
                                                    (\( user, millis ) newsum ->
                                                        case TDict.get user newsum of
                                                            Just oldsum ->
                                                                TDict.insert user (millis + oldsum) newsum

                                                            Nothing ->
                                                                TDict.insert user millis newsum
                                                    )
                                                    sum
                                            , distamt - daysum
                                            )
                                    )
                                    ( TR.emptyUserTimeDict, distmillis )
                    in
                    ( { model
                        | distribution =
                            Just
                                (Tuple.first dist
                                    |> TDict.map (\_ i -> millisAsHours i)
                                )
                      }
                    , None
                    )

                Nothing ->
                    ( model, None )

        ExportAll ->
            ( model
            , SaveCsv ("timeclonk-" ++ model.project.name ++ ".csv") (eteToCsv zone model.project.name model.membernames (getTes model.teamentries |> Dict.values))
            )

        TeamForward ->
            ( { model | teampaginator = P.onForward model.teampaginator }, None )

        TeamBack c ->
            ( { model | teampaginator = P.onBack c model.teampaginator }, None )

        TeamToStart ->
            ( { model | teampaginator = P.onToStart model.teampaginator }, None )

        TeamToEnd c ->
            ( { model | teampaginator = P.onToEnd c model.teampaginator }, None )

        PeForward ->
            ( { model | pepaginator = P.onForward model.pepaginator }, None )

        PeBack c ->
            ( { model | pepaginator = P.onBack c model.pepaginator }, None )

        PeToStart ->
            ( { model | pepaginator = P.onToStart model.pepaginator }, None )

        PeToEnd c ->
            ( { model | pepaginator = P.onToEnd (Dict.size model.payentries) model.pepaginator }, None )

        AForward ->
            ( { model | apaginator = P.onForward model.apaginator }, None )

        ABack c ->
            ( { model | apaginator = P.onBack c model.apaginator }, None )

        AToStart ->
            ( { model | apaginator = P.onToStart model.apaginator }, None )

        AToEnd c ->
            ( { model | apaginator = P.onToEnd c model.apaginator }, None )

        DForward ->
            ( { model | dpaginator = P.onForward model.dpaginator }, None )

        DBack c ->
            ( { model | dpaginator = P.onBack c model.dpaginator }, None )

        DToStart ->
            ( { model | dpaginator = P.onToStart model.dpaginator }, None )

        DToEnd c ->
            ( { model | dpaginator = P.onToEnd c model.dpaginator }, None )

        DonePress ->
            ( model, Done )

        Noop ->
            ( model, None )
