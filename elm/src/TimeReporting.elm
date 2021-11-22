module TimeReporting exposing (..)

import Calendar
import Clock
import DateTime
import Dict exposing (Dict)
import Round as R
import Time


type alias EditTimeEntry =
    { id : Maybe Int
    , user : Int
    , description : String
    , startdate : Int
    , enddate : Int
    , ignore : Bool
    , checked : Bool
    }


type alias EditPayEntry =
    { id : Maybe Int
    , user : Int
    , description : String
    , paymentdate : Int
    , duration : Int
    }


millisToHours : Int -> Float
millisToHours millis =
    toFloat millis
        / (1000 * 60 * 60)



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


millisPerDay : Time.Posix -> Time.Posix -> List ( Calendar.Date, Int )
millisPerDay from to =
    let
        fromdt =
            DateTime.fromPosix from

        todt =
            DateTime.fromPosix to

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


type alias Mpd =
    { date : Calendar.Date
    , user : Int
    , millis : Int
    }


userMillisPerDay : EditTimeEntry -> List Mpd
userMillisPerDay ete =
    millisPerDay (Time.millisToPosix ete.startdate) (Time.millisToPosix ete.enddate)
        |> List.map
            (\( date, millis ) ->
                { millis = millis
                , user = ete.user
                , date = date
                }
            )


teamMillisPerDay : List EditTimeEntry -> Dict Int (Dict Int Int)
teamMillisPerDay etes =
    let
        e : Dict Int (Dict Int Int)
        e =
            Dict.empty

        um : Dict Int Int
        um =
            Dict.empty
    in
    etes
        |> List.filter (.ignore >> not)
        |> List.foldl (\ete mpds -> userMillisPerDay ete ++ mpds) []
        |> List.foldl
            (\mpd dict ->
                let
                    mlis =
                        Calendar.toMillis mpd.date
                in
                case Dict.get mlis dict of
                    Just byuser ->
                        case Dict.get mpd.user byuser of
                            Just umillis ->
                                Dict.insert mlis (Dict.insert mpd.user (umillis + mpd.millis) byuser) dict

                            Nothing ->
                                Dict.insert mlis (Dict.insert mpd.user mpd.millis byuser) dict

                    Nothing ->
                        Dict.insert mlis (Dict.insert mpd.user mpd.millis um) dict
            )
            e


payTotes : List EditPayEntry -> Dict Int Int
payTotes entries =
    entries
        |> List.foldl
            (\entry sums ->
                case Dict.get entry.user sums of
                    Just sum ->
                        Dict.insert entry.user (sum + entry.duration) sums

                    Nothing ->
                        Dict.insert entry.user entry.duration sums
            )
            Dict.empty


timeTotes : List EditTimeEntry -> Dict Int Int
timeTotes entries =
    entries
        |> List.filter (.ignore >> not)
        |> List.foldl
            (\entry sums ->
                case Dict.get entry.user sums of
                    Just sum ->
                        Dict.insert entry.user (sum + entry.enddate - entry.startdate) sums

                    Nothing ->
                        Dict.insert entry.user (entry.enddate - entry.startdate) sums
            )
            Dict.empty
