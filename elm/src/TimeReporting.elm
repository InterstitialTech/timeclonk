module TimeReporting exposing (..)

import Calendar
import Clock
import Csv
import Data exposing (AllocationId, PayEntryId, PayType, TimeEntryId, getProjectIdVal)
import DateTime exposing (DateTime)
import Dict exposing (Dict)
import Orgauth.Data as OD exposing (UserId, getUserIdVal, makeUserId)
import Round as R
import TDict exposing (TDict)
import Time exposing (Zone)
import Toop
import Toop.Take as TT
import Util


type alias EditTimeEntry =
    { id : Maybe TimeEntryId
    , user : UserId
    , description : String
    , startdate : Int
    , enddate : Int
    , ignore : Bool
    , project : Data.ProjectId
    , checked : Bool
    }


type alias EditPayEntry =
    { id : Maybe PayEntryId
    , user : UserId
    , description : String
    , paytype : PayType
    , paymentdate : Int
    , duration : Int
    , checked : Bool
    }


type alias EditAllocation =
    { id : Maybe AllocationId
    , description : String
    , allocationdate : Int
    , duration : Int
    , checked : Bool
    }


millisToHours : Int -> Float
millisToHours millis =
    toFloat millis
        / (1000 * 60 * 60)


millisAsHours : Int -> String
millisAsHours millis =
    R.round 2 (toFloat millis / (1000.0 * 60.0 * 60.0))



-- totalMillis : {starttime, endtime} -> Int


eteMillis : EditTimeEntry -> Int
eteMillis ete =
    ete.enddate - ete.startdate


totalMillis : List EditTimeEntry -> Int
totalMillis etes =
    List.foldl
        (\ete sum ->
            if ete.ignore then
                sum

            else
                eteMillis ete + sum
        )
        0
        etes


toZoneDateTime : Time.Zone -> Time.Posix -> Maybe DateTime
toZoneDateTime zone posix =
    DateTime.fromRawParts
        { day = Time.toDay zone posix
        , month = Time.toMonth zone posix
        , year = Time.toYear zone posix
        }
        { hours = Time.toHour zone posix
        , minutes = Time.toMinute zone posix
        , seconds = Time.toSecond zone posix
        , milliseconds = Time.toMillis zone posix
        }


toDate : Time.Zone -> Time.Posix -> Maybe Calendar.Date
toDate zone posix =
    Calendar.fromRawParts
        { day = Time.toDay zone posix
        , month = Time.toMonth zone posix
        , year = Time.toYear zone posix
        }


millisPerDay : Time.Zone -> Time.Posix -> Time.Posix -> List ( Calendar.Date, Int )
millisPerDay zone from to =
    case ( toZoneDateTime zone from, toZoneDateTime zone to ) of
        ( Just fromdt, Just todt ) ->
            let
                fromdate =
                    DateTime.getDate fromdt

                todate =
                    DateTime.getDate todt

                msecstill date untildate =
                    if date == untildate then
                        []

                    else
                        ( date, 24 * 60 * 60 * 1000 )
                            :: msecstill (Calendar.incrementDay date) untildate
            in
            if fromdate == todate then
                [ ( DateTime.getDate fromdt
                  , (todt |> DateTime.getTime |> Clock.toMillis)
                        - (fromdt |> DateTime.getTime |> Clock.toMillis)
                  )
                ]

            else
                ( fromdate, 24 * 60 * 60 * 1000 - (fromdt |> DateTime.getTime |> Clock.toMillis) )
                    :: msecstill (Calendar.incrementDay fromdate) todate
                    ++ [ ( todate, todt |> DateTime.getTime |> Clock.toMillis ) ]

        _ ->
            []


type alias Mpd =
    { date : Calendar.Date
    , user : UserId
    , millis : Int
    }


userMillisPerDay : Zone -> EditTimeEntry -> List Mpd
userMillisPerDay zone ete =
    millisPerDay zone (Time.millisToPosix ete.startdate) (Time.millisToPosix ete.enddate)
        |> List.map
            (\( date, millis ) ->
                { millis = millis
                , user = ete.user
                , date = date
                }
            )


teamMillisPerDay : Zone -> List EditTimeEntry -> Dict Int (TDict UserId Int Int)
teamMillisPerDay zone etes =
    let
        e : Dict Int (TDict UserId Int Int)
        e =
            Dict.empty

        um : TDict UserId Int Int
        um =
            emptyUserTimeDict
    in
    etes
        |> List.filter (.ignore >> not)
        |> List.foldl (\ete mpds -> userMillisPerDay zone ete ++ mpds) []
        |> List.foldl
            (\mpd dict ->
                let
                    mlis =
                        Calendar.toMillis mpd.date
                in
                case Dict.get mlis dict of
                    Just byuser ->
                        case TDict.get mpd.user byuser of
                            Just umillis ->
                                Dict.insert mlis (TDict.insert mpd.user (umillis + mpd.millis) byuser) dict

                            Nothing ->
                                Dict.insert mlis (TDict.insert mpd.user mpd.millis byuser) dict

                    Nothing ->
                        Dict.insert mlis (TDict.insert mpd.user mpd.millis um) dict
            )
            e


payTotes : List EditPayEntry -> TDict UserId Int Int
payTotes entries =
    entries
        |> List.foldl
            (\entry sums ->
                case TDict.get entry.user sums of
                    Just sum ->
                        TDict.insert entry.user (sum + entry.duration) sums

                    Nothing ->
                        TDict.insert entry.user entry.duration sums
            )
            emptyUserTimeDict


dayTotes : Zone -> List EditTimeEntry -> Dict Int Int
dayTotes zone timeentries =
    timeentries
        |> List.foldl
            (\te ddict ->
                millisPerDay zone (Time.millisToPosix te.startdate) (Time.millisToPosix te.enddate)
                    |> (\lst ->
                            if te.ignore then
                                List.map (\( date, millis ) -> ( date, 0 )) lst

                            else
                                lst
                       )
                    |> List.foldl
                        (\( date, millis ) ddicttoo ->
                            let
                                dmils =
                                    Calendar.toMillis date
                            in
                            case Dict.get dmils ddict of
                                Just totemillis ->
                                    Dict.insert dmils (totemillis + millis) ddict

                                Nothing ->
                                    Dict.insert dmils millis ddict
                        )
                        ddict
            )
            Dict.empty


addDays : Calendar.Date -> Int -> Calendar.Date
addDays date days =
    let
        millis =
            Time.posixToMillis (Time.millisToPosix (1000 * 60 * 60 * 12 + Calendar.toMillis date)) + (Calendar.millisInADay * days)

        newDate =
            Calendar.fromPosix (Time.millisToPosix millis)
    in
    newDate


dayIndex : Calendar.Date -> Int
dayIndex date =
    case Calendar.getWeekday date of
        Time.Sun ->
            0

        Time.Mon ->
            1

        Time.Tue ->
            2

        Time.Wed ->
            3

        Time.Thu ->
            4

        Time.Fri ->
            5

        Time.Sat ->
            6


toSunday : Calendar.Date -> Calendar.Date
toSunday date =
    addDays date (dayIndex date * -1)


weekTotes : Zone -> List EditTimeEntry -> Dict Int Int
weekTotes zone timeentries =
    timeentries
        |> List.foldl
            (\te ddict ->
                millisPerDay zone (Time.millisToPosix te.startdate) (Time.millisToPosix te.enddate)
                    |> (\lst ->
                            if te.ignore then
                                List.map (\( date, millis ) -> ( date, 0 )) lst

                            else
                                lst
                       )
                    |> List.foldl
                        (\( date, millis ) ddicttoo ->
                            let
                                wmils =
                                    Calendar.toMillis
                                        (toSunday date)
                            in
                            case Dict.get wmils ddict of
                                Just totemillis ->
                                    Dict.insert wmils (totemillis + millis) ddict

                                Nothing ->
                                    Dict.insert wmils millis ddict
                        )
                        ddict
            )
            Dict.empty


emptyUmDict : TDict UserId Int Data.User
emptyUmDict =
    TDict.empty getUserIdVal makeUserId


emptyUserTimeDict : TDict UserId Int Int
emptyUserTimeDict =
    TDict.empty getUserIdVal makeUserId


timeTotes : List EditTimeEntry -> TDict UserId Int Int
timeTotes entries =
    entries
        |> List.filter (.ignore >> not)
        |> List.foldl
            (\entry sums ->
                case TDict.get entry.user sums of
                    Just sum ->
                        TDict.insert entry.user (sum + entry.enddate - entry.startdate) sums

                    Nothing ->
                        TDict.insert entry.user (entry.enddate - entry.startdate) sums
            )
            emptyUserTimeDict


csvToEditTimeEntries : Time.Zone -> UserId -> Data.ProjectId -> Csv.Csv -> Result (List String) (List EditTimeEntry)
csvToEditTimeEntries zone user projectid csv =
    let
        headers =
            List.map (String.trim >> String.toLower) csv.headers
    in
    case TT.takeT3 headers of
        Just ( Toop.T3 "task" "from" "to", _ ) ->
            let
                resitems =
                    csv.records
                        |> List.map (\lst -> List.map String.trim lst)
                        |> List.foldl
                            (\row rlst ->
                                rlst
                                    |> Result.andThen
                                        (\lst ->
                                            case TT.takeT3 row of
                                                Nothing ->
                                                    Err [ "each row requires 3 entries: task description, from date, and to date." ]

                                                Just ( Toop.T3 task dtfrom dtto, _ ) ->
                                                    let
                                                        rsfrom =
                                                            Util.parseTime zone dtfrom

                                                        rsto =
                                                            Util.parseTime zone dtto
                                                    in
                                                    case ( rsfrom, rsto ) of
                                                        ( Ok (Just from), Ok (Just to) ) ->
                                                            Ok <|
                                                                { id = Nothing
                                                                , user = user
                                                                , description = task
                                                                , startdate = Time.posixToMillis from
                                                                , enddate = Time.posixToMillis to
                                                                , project = projectid
                                                                , ignore = False
                                                                , checked = False
                                                                }
                                                                    :: lst

                                                        ( Err e, _ ) ->
                                                            Err [ Util.deadEndsToString e ]

                                                        ( _, Err e ) ->
                                                            Err [ Util.deadEndsToString e ]

                                                        _ ->
                                                            Err [ "invalid date" ]
                                        )
                            )
                            (Ok [])
            in
            resitems

        _ ->
            Err [ "3 header columns required: 'task', 'from' and 'to'." ]


{-| just export the checked Etes
-}
eteToCsv : Time.Zone -> Dict Int String -> Dict Int String -> List EditTimeEntry -> String
eteToCsv zone projectnames membernames timeentries =
    ("project,user,task,startdate,enddate,duration"
        :: (timeentries
                |> List.map
                    (\te ->
                        "\""
                            ++ (Dict.get (getProjectIdVal te.project) projectnames
                                    |> Maybe.withDefault ""
                               )
                            ++ "\",\""
                            ++ (Dict.get (getUserIdVal te.user) membernames
                                    |> Maybe.withDefault ""
                               )
                            ++ "\",\""
                            ++ te.description
                            ++ "\",\""
                            ++ Util.showDateTime zone (Time.millisToPosix te.startdate)
                            ++ "\",\""
                            ++ Util.showDateTime zone (Time.millisToPosix te.enddate)
                            ++ "\","
                            ++ (te.enddate - te.startdate |> millisAsHours)
                    )
           )
    )
        |> List.intersperse "\n"
        |> String.concat


csvToEditAllocations : Time.Zone -> UserId -> Csv.Csv -> Result (List String) (List EditAllocation)
csvToEditAllocations zone user csv =
    let
        headers =
            List.map (String.trim >> String.toLower) csv.headers
    in
    case TT.takeT3 headers of
        Just ( Toop.T3 "description" "date" "hours", _ ) ->
            let
                resitems =
                    csv.records
                        |> List.map (\lst -> List.map String.trim lst)
                        |> List.foldl
                            (\row rlst ->
                                rlst
                                    |> Result.andThen
                                        (\lst ->
                                            case TT.takeT3 row of
                                                Nothing ->
                                                    Err [ "each allocation row requires 3 entries: description, date, and decimal hours." ]

                                                Just ( Toop.T3 description sdate shours, _ ) ->
                                                    let
                                                        pdate =
                                                            Util.parseTime zone sdate

                                                        phours =
                                                            String.toFloat shours
                                                    in
                                                    case ( pdate, phours ) of
                                                        ( Ok (Just date), Just hours ) ->
                                                            Ok <|
                                                                { id = Nothing
                                                                , description = description
                                                                , allocationdate = Time.posixToMillis date
                                                                , duration = hours * 60 * 60 * 1000 |> round
                                                                , checked = False
                                                                }
                                                                    :: lst

                                                        ( Err e, _ ) ->
                                                            Err [ Util.deadEndsToString e ]

                                                        ( _, Nothing ) ->
                                                            Err [ "invalid hours: " ++ shours ]

                                                        _ ->
                                                            Err [ "invalid date" ]
                                        )
                            )
                            (Ok [])
            in
            resitems

        _ ->
            Err [ "3 header columns required: 'description', 'date', and 'hours'." ]
