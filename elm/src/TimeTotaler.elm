module TimeTotaler exposing (TTotaler, getTes, getTotes, mapTimeentry, mkTToteler, setTes)

import Data exposing (UserId)
import Dict exposing (Dict(..))
import Set
import Time
import TimeReporting as TR exposing (EditAllocation, EditPayEntry, EditTimeEntry, csvToEditAllocations, csvToEditTimeEntries, eteToCsv)


type alias TimeTotes =
    { mytimeentries : List EditTimeEntry
    , daytotes : Dict Int Int
    , lastofdays : Set.Set Int
    , lastofweeks : Set.Set Int
    , lasttime : Maybe Int
    , myhours : Float
    , teamhours : Float
    , weektotes : Dict Int Int
    , userid : UserId
    , zone : Time.Zone
    }



-- the constructor for this is NOT public.
-- if its always built with mkTToteler or setTes, then the totals will always be consistent.


type TTotaler
    = TTotaler (Dict Int EditTimeEntry) TimeTotes


getTes : TTotaler -> Dict Int EditTimeEntry
getTes tt =
    case tt of
        TTotaler dict _ ->
            dict


getTotes : TTotaler -> TimeTotes
getTotes tt =
    case tt of
        TTotaler _ totes ->
            totes


setTes : TTotaler -> Dict Int EditTimeEntry -> TTotaler
setTes ttotl timeentries =
    case ttotl of
        TTotaler te ttot ->
            mkTToteler timeentries ttot.userid ttot.zone


mapTimeentry : TTotaler -> Int -> (EditTimeEntry -> EditTimeEntry) -> TTotaler
mapTimeentry ttot startdate f =
    case Dict.get startdate (getTes ttot) of
        Just te ->
            setTes ttot
                (Dict.insert startdate (f te) <| getTes ttot)

        Nothing ->
            ttot


mkTToteler : Dict Int EditTimeEntry -> UserId -> Time.Zone -> TTotaler
mkTToteler timeentries userid zone =
    let
        teamhours =
            timeentries |> Dict.values |> TR.totalMillis |> TR.millisToHours

        mytimeentries =
            timeentries
                |> Dict.values
                |> List.filter (\te -> te.user == userid)

        myhours =
            mytimeentries
                |> TR.totalMillis
                |> TR.millisToHours

        daytotes =
            mytimeentries
                |> TR.dayTotes zone

        weektotes =
            mytimeentries
                |> TR.weekTotes zone

        lasttime =
            mytimeentries
                |> List.reverse
                |> List.head
                |> Maybe.map .startdate

        ( lastofdays, _ ) =
            mytimeentries
                |> List.reverse
                |> List.foldl
                    (\te ( set, pd ) ->
                        case TR.toDate zone (Time.millisToPosix te.startdate) of
                            Just cd ->
                                if Just cd == pd then
                                    ( set, pd )

                                else
                                    ( Set.insert te.startdate set, Just cd )

                            Nothing ->
                                ( set, pd )
                    )
                    ( Set.empty, Nothing )

        ( lastofweeks, _ ) =
            mytimeentries
                |> List.reverse
                |> List.foldl
                    (\te ( set, pd ) ->
                        case TR.toDate zone (Time.millisToPosix te.startdate) |> Maybe.map TR.toSunday of
                            Just cd ->
                                if Just cd == pd then
                                    ( set, pd )

                                else
                                    ( Set.insert te.startdate set, Just cd )

                            Nothing ->
                                ( set, pd )
                    )
                    ( Set.empty, Nothing )
    in
    TTotaler timeentries
        { mytimeentries = mytimeentries
        , teamhours = teamhours
        , myhours = myhours
        , daytotes = daytotes
        , weektotes = weektotes
        , lasttime = lasttime
        , lastofdays = lastofdays
        , lastofweeks = lastofweeks
        , userid = userid
        , zone = zone
        }
