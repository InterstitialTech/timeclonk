module UserTime exposing (..)

import Calendar
import Common
import Csv
import Data
import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Background as EBk
import Element.Border as EBd
import Element.Events as EE
import Element.Font as EF
import Element.Input as EI
import Orgauth.Data as OD exposing (UserId, getUserIdVal, makeUserId)
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
    | TeForward
    | TeBack Int
    | TeToStart
    | TeToEnd Int
    | Noop


type alias Model =
    { projects : Dict Int Data.ListProject
    , timeentries : TTotaler
    , initialtimeentries : Dict Int EditTimeEntry
    , tepaginator : P.Model Msg
    }


type Command
    = Done
    | SaveCsv String String
    | Settings
    | ShowError String
    | None


headerStyle : List (E.Attribute msg)
headerStyle =
    [ EF.bold ]


emptyTimeEntryIdSet : TSet Data.TimeEntryId Int
emptyTimeEntryIdSet =
    TSet.empty Data.getTimeEntryIdVal Data.makeTimeEntryId


toEditTimeEntry : Data.TimeEntry -> EditTimeEntry
toEditTimeEntry te =
    { id = Just te.id
    , user = te.user
    , description = te.description
    , startdate = te.startdate
    , enddate = te.enddate
    , ignore = te.ignore
    , project = te.project
    , checked = False
    }


toEteDict : List Data.TimeEntry -> Dict Int EditTimeEntry
toEteDict te =
    te
        |> List.map (toEditTimeEntry >> (\ete -> ( ete.startdate, ete )))
        |> Dict.fromList


init : Time.Zone -> Data.LoginData -> List Data.TimeEntry -> Dict Int Data.ListProject -> Int -> Model
init zone ld timeentries projects pageincrement =
    let
        ietes =
            toEteDict timeentries
    in
    { projects = projects
    , timeentries = mkTToteler ietes (\te -> te.user == ld.userid) zone
    , initialtimeentries = ietes
    , tepaginator = P.init TeForward TeBack TeToStart TeToEnd P.End pageincrement
    }


setPageIncrement : Int -> Model -> Model
setPageIncrement pageincrement model =
    let
        tp =
            model.tepaginator
    in
    { model
        | tepaginator = { tp | pageincrement = pageincrement }
    }


view : Data.LoginData -> Util.Size -> Time.Zone -> Model -> Element Msg
view ld size zone model =
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
                [ E.row [ EF.bold ] [ E.text ld.name ]
                , EI.button
                    (E.alignRight :: Common.buttonStyle)
                    { onPress = Just SettingsPress, label = E.text "settings" }
                ]
            , E.row [ E.spacing TC.defaultSpacing ] [ E.text "project:", E.el [ EF.bold ] <| E.text ld.name ]
            , E.row [ E.spacing TC.defaultSpacing ] <|
                [ EI.button Common.buttonStyle { onPress = Just DonePress, label = E.text "<-" }
                , EI.button Common.buttonStyle { onPress = Just ExportAll, label = E.text "export" }
                ]
            ]
                ++ clonkview ld size zone model


dateTimeWidth =
    200


clonkview : Data.LoginData -> Util.Size -> Time.Zone -> Model -> List (Element Msg)
clonkview ld size zone model =
    let
        ttotes =
            getTotes model.timeentries

        igfont =
            \te ->
                if te.ignore then
                    EF.strike

                else
                    EF.regular
    in
    [ P.view ttotes.mtecount model.tepaginator
    , E.table [ E.spacing TC.defaultSpacing, E.width E.fill ]
        { data =
            P.filter model.tepaginator ttotes.mytimeentries
        , columns =
            [ { header = E.el headerStyle <| E.text "project"
              , width = E.fill
              , view =
                    \te ->
                        E.row
                            [ E.width E.fill
                            , E.height E.fill
                            ]
                            [ E.text <|
                                let
                                    pid =
                                        Data.getProjectIdVal te.project
                                in
                                Dict.get pid model.projects
                                    |> Maybe.map .name
                                    |> Maybe.withDefault (String.fromInt pid)
                            ]
              }
            , { header = E.el headerStyle <| E.text "task"
              , width = E.fill
              , view =
                    \te ->
                        E.row
                            [ E.width E.fill
                            , E.height E.fill
                            ]
                            [ E.text <|
                                if te.description == "" then
                                    " "

                                else
                                    te.description
                            ]
              }
            , { header = E.el headerStyle <| E.text "start"
              , width = E.fill
              , view =
                    \te ->
                        E.row
                            [ E.width E.fill
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
                            [ E.width E.fill
                            ]
                            [ E.text <| endtext te.enddate
                            ]
              }
            , { header = E.el headerStyle <| E.text "duration"
              , width = E.shrink
              , view =
                    \te ->
                        E.row
                            [ E.width E.fill
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
                                    E.el
                                        []
                                    <|
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
                                    E.el
                                        []
                                    <|
                                        E.text (millisAsHours millis)

                                Nothing ->
                                    E.none

                        else
                            E.none
              }
            ]
        }
    ]


update : Msg -> Model -> Data.LoginData -> Time.Zone -> ( Model, Command )
update msg model ld zone =
    case msg of
        SettingsPress ->
            ( model, Settings )

        ExportAll ->
            ( model
            , SaveCsv ("timeclonk-" ++ ld.name ++ ".csv")
                (eteToCsv zone
                    (Dict.map (\_ v -> v.name) model.projects)
                    (Dict.fromList [ ( OD.getUserIdVal ld.userid, ld.name ) ])
                    (getTes model.timeentries |> Dict.values)
                )
            )

        TeForward ->
            ( { model | tepaginator = P.onForward model.tepaginator }, None )

        TeBack c ->
            ( { model | tepaginator = P.onBack c model.tepaginator }, None )

        TeToStart ->
            ( { model | tepaginator = P.onToStart model.tepaginator }, None )

        TeToEnd c ->
            ( { model | tepaginator = P.onToEnd c model.tepaginator }, None )

        DonePress ->
            ( model, Done )

        Noop ->
            ( model, None )
