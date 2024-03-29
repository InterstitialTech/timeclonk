module TimeTotaler exposing (TTotaler, getTes, getTotes, mapTimeentry, mkTToteler, setTes)

import Data
import Dict exposing (Dict(..))
import Orgauth.Data as OD exposing (UserId, getUserIdVal, makeUserId)
import Set
import Time
import TimeReporting as TR exposing (EditAllocation, EditPayEntry, EditTimeEntry, csvToEditAllocations, csvToEditTimeEntries, eteToCsv)


type alias TimeTotes =
    { mytimeentries : List EditTimeEntry
    , mtecount : Int
    , daytotes : Dict Int Int
    , lastofdays : Set.Set Int
    , lastofweeks : Set.Set Int
    , lasttime : Maybe Int
    , myhours : Float
    , teamhours : Float
    , teammillis : Int
    , weektotes : Dict Int Int
    , filterf : FilterF
    , zone : Time.Zone
    }


type alias FilterF =
    EditTimeEntry -> Bool



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
            mkTToteler timeentries ttot.filterf ttot.zone


mapTimeentry : TTotaler -> Int -> (EditTimeEntry -> EditTimeEntry) -> TTotaler
mapTimeentry ttot startdate f =
    case Dict.get startdate (getTes ttot) of
        Just te ->
            setTes ttot
                (Dict.insert startdate (f te) <| getTes ttot)

        Nothing ->
            ttot


mkTToteler : Dict Int EditTimeEntry -> FilterF -> Time.Zone -> TTotaler
mkTToteler timeentries filterf zone =
    let
        teammillis =
            timeentries |> Dict.values |> TR.totalMillis

        teamhours =
            TR.millisToHours teammillis

        mytimeentries =
            timeentries
                |> Dict.values
                |> List.filter filterf

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
        , mtecount = List.length mytimeentries
        , teamhours = teamhours
        , teammillis = teammillis
        , myhours = myhours
        , daytotes = daytotes
        , weektotes = weektotes
        , lasttime = lasttime
        , lastofdays = lastofdays
        , lastofweeks = lastofweeks
        , filterf = filterf
        , zone = zone
        }
