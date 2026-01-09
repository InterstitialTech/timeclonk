module TaskSummary exposing (..)

import Common
import Data exposing (PayType(..))
import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Font as EF
import Element.Input as EI
import Orgauth.Data exposing (UserId)
import Round as R
import TcCommon as TC
import TimeReporting exposing (EditPayEntry, EditTimeEntry, descriptionSummary, invoiceTotes, paidEntries, payTotes, unpaidEntries)
import TimeTotaler exposing (TTotaler, getTes)


type TimeType
    = Paid
    | Unpaid
    | Invoiced
    | Uninvoiced
    | All


type Sort
    = Description
    | Hours


type Direction
    = Asc
    | Desc


type alias Model =
    { users : List UserId
    , timetype : TimeType
    , sort : Sort
    , direction : Direction
    }


type Msg
    = Noop
    | CheckMe Bool
    | SelectTimeType TimeType
    | ToggleHours
    | ToggleDescription


default : Model
default =
    { users = []
    , timetype = All
    , sort = Hours
    , direction = Desc
    }


opdir : Direction -> Direction
opdir dir =
    case dir of
        Asc ->
            Desc

        Desc ->
            Asc


update : Msg -> UserId -> Model -> Model
update msg me model =
    case msg of
        Noop ->
            model

        CheckMe team ->
            { model
                | users =
                    if team then
                        []

                    else
                        [ me ]
            }

        SelectTimeType tt ->
            { model | timetype = tt }

        ToggleHours ->
            let
                ( sort, direction ) =
                    case ( model.sort, model.direction ) of
                        ( Hours, dir ) ->
                            ( Hours, opdir dir )

                        ( Description, dir ) ->
                            ( Hours, dir )
            in
            { model | sort = sort, direction = direction }

        ToggleDescription ->
            let
                ( sort, direction ) =
                    case ( model.sort, model.direction ) of
                        ( Hours, dir ) ->
                            ( Description, dir )

                        ( Description, dir ) ->
                            ( Description, opdir dir )
            in
            { model | sort = sort, direction = direction }


taskFilterView : UserId -> Model -> Element Msg
taskFilterView me model =
    E.column []
        [ EI.checkbox [ E.width E.shrink, E.centerY ]
            { onChange = CheckMe
            , icon = TC.checkboxIcon
            , checked = model.users /= [ me ]
            , label = EI.labelLeft [] (E.text "team")
            }
        , EI.radio []
            { onChange = SelectTimeType
            , options =
                [ EI.option Paid (E.text "paid")
                , EI.option Unpaid (E.text "unpaid")
                , EI.option Invoiced (E.text "invoiced")
                , EI.option Uninvoiced (E.text "uninvoiced")
                , EI.option All (E.text "all")
                ]
            , selected = Just model.timetype
            , label = EI.labelLeft [] (E.text "time type")
            }
        ]


taskview : TTotaler -> Dict Int EditPayEntry -> Dict Int String -> Model -> Element Msg
taskview timeentries payentries membernames model =
    let
        tedict =
            if model.users == [] then
                getTes timeentries

            else
                getTes timeentries
                    |> Dict.filter (\_ v -> List.any ((==) v.user) model.users)

        tes : List EditTimeEntry
        tes =
            case model.timetype of
                Paid ->
                    paidEntries tedict (payTotes (Dict.values payentries))

                Unpaid ->
                    unpaidEntries tedict (payTotes (Dict.values payentries))

                Invoiced ->
                    paidEntries tedict (invoiceTotes (Dict.values payentries))

                Uninvoiced ->
                    unpaidEntries tedict (invoiceTotes (Dict.values payentries))

                All ->
                    Dict.values <| tedict
    in
    E.table [ E.spacing TC.defaultSpacing, E.width E.fill ]
        { data =
            descriptionSummary membernames tes
                |> Dict.toList
                |> (case ( model.sort, model.direction ) of
                        ( Description, Asc ) ->
                            List.sortBy Tuple.first

                        ( Description, Desc ) ->
                            \a -> List.sortBy Tuple.first a |> List.reverse

                        ( Hours, Asc ) ->
                            List.sortBy Tuple.second

                        ( Hours, Desc ) ->
                            \a -> List.sortBy Tuple.second a |> List.reverse
                   )
        , columns =
            [ { header =
                    EI.button
                        Common.buttonStyle
                        { onPress = Just ToggleDescription
                        , label =
                            case ( model.sort, model.direction ) of
                                ( Description, Asc ) ->
                                    E.row []
                                        [ E.el [ EF.underline, EF.bold ] <| E.text "task"
                                        , E.el [ EF.bold ] <| E.text "↑"
                                        ]

                                ( Description, Desc ) ->
                                    E.row []
                                        [ E.el [ EF.underline, EF.bold ] <| E.text "task"
                                        , E.el [ EF.bold ] <| E.text "↓"
                                        ]

                                ( Hours, Asc ) ->
                                    E.el [ EF.underline ] <| E.text "task"

                                ( Hours, Desc ) ->
                                    E.el [ EF.underline ] <| E.text "task"
                        }
              , width = E.shrink
              , view =
                    \( task, _ ) ->
                        E.text task
              }
            , { header =
                    EI.button
                        Common.buttonStyle
                        { onPress = Just ToggleHours
                        , label =
                            case ( model.sort, model.direction ) of
                                ( Description, Asc ) ->
                                    E.el [ EF.underline ] <| E.text "hours"

                                ( Description, Desc ) ->
                                    E.el [ EF.underline ] <| E.text "hours"

                                ( Hours, Asc ) ->
                                    E.row []
                                        [ E.el [ EF.underline, EF.bold ] <| E.text "hours"
                                        , E.el [ EF.bold ] <| E.text "↑"
                                        ]

                                ( Hours, Desc ) ->
                                    E.row []
                                        [ E.el [ EF.underline, EF.bold ] <| E.text "hours"
                                        , E.el [ EF.bold ] <| E.text "↓"
                                        ]
                        }
              , width = E.shrink
              , view =
                    \( _, millis ) ->
                        E.text (R.round 2 <| toFloat millis / 1000 / 60 / 60)
              }
            ]
        }
