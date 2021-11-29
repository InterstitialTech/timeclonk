module ProjectTime exposing (..)

import Calendar
import Common
import Csv
import Data exposing (UserId)
import Dialog as D
import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Background as EBk
import Element.Border as EBd
import Element.Events as EE
import Element.Font as EF
import Element.Input as EI
import Element.Keyed as EK
import Element.Region
import File exposing (File)
import File.Select as FS
import Round as R
import SelectString
import Set
import TDict exposing (TDict)
import TSet exposing (TSet)
import TangoColors as TC
import TcCommon as TC
import Time
import TimeReporting as TR exposing (EditPayEntry, EditTimeEntry)
import Toop
import Toop.Take as TT
import Util
import WindowKeys as WK


type Msg
    = DescriptionChanged String
    | SavePress
    | RevertPress
    | DonePress
    | EditPress
    | SettingsPress
    | ImportPress
    | CsvString String
    | ClonkInPress
    | ClonkOutPress
    | ClonkInTime Int
    | ClonkOutTime Int
    | DeleteClonk Int
    | DeletePay Int
    | EteDescriptionChanged Int String
    | EteStartChanged Int String
    | FocusDescriptionChanged String
    | FocusStartChanged String
    | FocusEndChanged String
    | FocusDurationChanged String
    | FocusPayChanged String
    | FocusPayDateChanged String
    | ChangeStart Int
    | ChangePayDate Int
    | SetViewMode ViewMode
    | OnRowItemClick Int FocusColumn
    | OnDistributionChanged String
    | ClearDistribution
    | CalcDistribution
    | OnPaymentChanged UserId String
    | AddPaymentPress UserId Int
    | AddPayment UserId Int Int
    | CheckAll Bool
    | CheckItem Int Bool
    | CheckPayAll Bool
    | CheckPayItem Int Bool
    | DeleteChecked
    | DeletePayChecked
    | IgnoreChecked
    | Noop


type ViewMode
    = Clonk
    | Payment


type FocusColumn
    = Description
    | Start
    | End
    | Duration
    | PaymentDate
    | PaymentAmount


type alias Model =
    { project : Data.Project
    , members : List Data.ProjectMember
    , description : String
    , timeentries : Dict Int EditTimeEntry
    , initialtimeentries : Dict Int EditTimeEntry
    , payentries : Dict Int EditPayEntry
    , initialpayentries : Dict Int EditPayEntry
    , focusstart : String
    , focusend : String
    , focusduration : String
    , focusdescription : String
    , focus : Maybe ( Int, FocusColumn )
    , focuspay : String
    , focuspaydate : String
    , distributionhours : String
    , distribution : Maybe (TDict UserId Int String)
    , viewmode : ViewMode
    }


type Command
    = Save Data.SaveProjectTime
    | Edit
    | Done
    | GetTime (Int -> Msg)
    | GetCsv
    | Settings
    | ShowError String
    | None


emptyTimeEntryIdSet : TSet Data.TimeEntryId Int
emptyTimeEntryIdSet =
    TSet.empty Data.getTimeEntryIdVal Data.makeTimeEntryId


emptyPayEntryIdSet : TSet Data.PayEntryId Int
emptyPayEntryIdSet =
    TSet.empty Data.getPayEntryIdVal Data.makePayEntryId


onWkKeyPress : WK.Key -> Model -> Data.LoginData -> Time.Zone -> ( Model, Command )
onWkKeyPress key model ld zone =
    case Toop.T4 key.key key.ctrl key.alt key.shift of
        Toop.T4 "s" True False False ->
            if isDirty model then
                update SavePress model ld zone

            else
                ( model, None )

        _ ->
            ( model, None )


toSaveProjectTime : Model -> Data.SaveProjectTime
toSaveProjectTime model =
    let
        savetimeentries =
            model.timeentries
                |> Dict.values
                |> List.foldl
                    (\te saves ->
                        case Dict.get te.startdate model.initialtimeentries of
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

        deletetimeentries =
            Dict.diff model.initialtimeentries model.timeentries
                |> Dict.values
                |> List.filterMap .id
                |> TSet.insertList emptyTimeEntryIdSet

        savepayentries =
            model.payentries
                |> Dict.values
                |> List.foldl
                    (\pe saves ->
                        case Dict.get pe.paymentdate model.initialpayentries of
                            Just ipe ->
                                if pe /= ipe then
                                    pe :: saves

                                else
                                    saves

                            Nothing ->
                                pe :: saves
                    )
                    []
                |> List.map (toSavePayEntry model)

        deletepayentries =
            Dict.diff model.initialpayentries model.payentries
                |> Dict.values
                |> List.filterMap .id
                |> TSet.insertList emptyPayEntryIdSet
    in
    { project = model.project.id
    , savetimeentries = savetimeentries

    -- remove update ids from the delete list.
    , deletetimeentries =
        List.foldl
            (\ste dte ->
                case ste.id of
                    Just id ->
                        TSet.remove id dte

                    Nothing ->
                        dte
            )
            deletetimeentries
            savetimeentries
            |> TSet.toList
    , savepayentries = savepayentries

    -- remove update ids from the delete list.
    , deletepayentries =
        List.foldl
            (\ste dte ->
                case ste.id of
                    Just id ->
                        TSet.remove id dte

                    Nothing ->
                        dte
            )
            deletepayentries
            savepayentries
            |> TSet.toList
    }


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


toSaveTimeEntry : Model -> EditTimeEntry -> Data.SaveTimeEntry
toSaveTimeEntry model ete =
    { id = ete.id
    , project = model.project.id
    , user = ete.user
    , description = ete.description
    , startdate = ete.startdate
    , enddate = ete.enddate
    , ignore = ete.ignore
    }


toSavePayEntry : Model -> EditPayEntry -> Data.SavePayEntry
toSavePayEntry model ete =
    { id = ete.id
    , project = model.project.id
    , user = ete.user
    , description = ete.description
    , paymentdate = ete.paymentdate
    , duration = ete.duration
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
    (model.timeentries |> Dict.map (\_ te -> { te | checked = False }))
        /= model.initialtimeentries
        || (model.payentries |> Dict.map (\_ pe -> { pe | checked = False }))
        /= model.initialpayentries


init : Data.LoginData -> Data.ProjectTime -> Model
init ld pt =
    let
        ietes =
            toEteDict pt.timeentries

        iepes =
            toEpeDict pt.payentries

        description =
            ietes |> Dict.toList |> List.filter (\( _, e ) -> e.user == ld.userid) |> List.reverse |> List.head |> Maybe.map (\( _, ete ) -> ete.description) |> Maybe.withDefault ""
    in
    { project = pt.project
    , members = pt.members
    , description = description
    , timeentries = ietes
    , initialtimeentries = ietes
    , payentries = iepes
    , initialpayentries = iepes
    , viewmode = Clonk
    , focusstart = ""
    , focusend = ""
    , focusduration = ""
    , focusdescription = ""
    , focus = Nothing
    , focuspay = ""
    , focuspaydate = ""
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
            , E.row [ E.spacing 8 ] <|
                [ EI.button Common.buttonStyle { onPress = Just DonePress, label = E.text "<-" }
                , EI.button Common.buttonStyle { onPress = Just EditPress, label = E.text "edit project" }
                , EI.button Common.buttonStyle { onPress = Just ImportPress, label = E.text "import" }
                ]
                    ++ (if isdirty then
                            [ EI.button Common.buttonStyle { onPress = Just RevertPress, label = E.text "revert" }
                            , EI.button
                                (Common.buttonStyle ++ [ EBk.color TC.darkYellow ])
                                { onPress = Just SavePress, label = E.text "save" }
                            ]

                        else
                            []
                       )
            , viewModeBar model
            ]
                ++ (case model.viewmode of
                        Clonk ->
                            clonkview ld size zone model

                        Payment ->
                            payview ld size zone model
                   )


clonkview : Data.LoginData -> Util.Size -> Time.Zone -> Model -> List (Element Msg)
clonkview ld size zone model =
    let
        teamhours =
            model.timeentries |> Dict.values |> TR.totalMillis |> TR.millisToHours

        myhours =
            model.timeentries
                |> Dict.values
                |> List.filter (\te -> te.user == ld.userid)
                |> TR.totalMillis
                |> TR.millisToHours

        paytotes =
            model.payentries |> Dict.values |> TR.payTotes

        mypay =
            paytotes
                |> TDict.get ld.userid
                |> Maybe.withDefault 0
                |> TR.millisToHours

        teampay =
            paytotes
                |> TDict.values
                |> List.foldl (+) 0
                |> TR.millisToHours

        igfont =
            \te ->
                if te.ignore then
                    EF.strike

                else
                    EF.regular

        anychecked =
            Dict.foldl (\_ te c -> c || te.checked) False model.timeentries
    in
    [ if anychecked then
        E.row [ E.spacing 8 ]
            [ E.text "checked items: "
            , EI.button Common.buttonStyle
                { onPress = Just <| DeleteChecked
                , label = E.text "delete"
                }
            , EI.button Common.buttonStyle
                { onPress = Just <| IgnoreChecked
                , label = E.text "ignore"
                }
            ]

      else
        E.none
    , E.table [ E.spacing 8, E.width E.fill ]
        { data = model.timeentries |> Dict.values |> List.filter (\te -> te.user == ld.userid)
        , columns =
            [ { header =
                    EI.checkbox [ E.width E.shrink ]
                        { onChange = CheckAll
                        , icon = EI.defaultCheckbox
                        , checked =
                            Dict.foldl
                                (\_ te ac ->
                                    if te.user == ld.userid then
                                        ac && te.checked

                                    else
                                        ac
                                )
                                True
                                model.timeentries
                        , label = EI.labelHidden "check all"
                        }
              , width = E.shrink
              , view =
                    \te ->
                        EI.checkbox [ E.width E.shrink ]
                            { onChange = CheckItem te.startdate
                            , icon = EI.defaultCheckbox
                            , checked = te.checked
                            , label = EI.labelHidden "check item"
                            }
              }
            , { header = E.text "Task"
              , width = E.fill
              , view =
                    \te ->
                        let
                            row =
                                E.row [ EE.onClick <| OnRowItemClick te.startdate Description, igfont te ]
                                    [ E.text te.description ]
                        in
                        if model.focus == Just ( te.startdate, Description ) then
                            E.column
                                [ E.spacing 8
                                ]
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
                                E.row [ EE.onClick <| OnRowItemClick te.startdate Start, igfont te ] [ E.text <| Util.showTime zone (Time.millisToPosix te.startdate) ]
                        in
                        if model.focus == Just ( te.startdate, Start ) then
                            let
                                ( display, mbstart ) =
                                    case Util.parseTime zone model.focusstart of
                                        Err e ->
                                            ( Util.deadEndsToString e, Nothing )

                                        Ok Nothing ->
                                            ( "invalid", Nothing )

                                        Ok (Just dt) ->
                                            ( Util.showTime zone dt, Just dt )
                            in
                            E.column [ E.spacing 8 ]
                                [ row
                                , EI.text [ E.width E.fill ]
                                    { onChange = FocusStartChanged
                                    , text = model.focusstart
                                    , placeholder = Nothing
                                    , label = EI.labelHidden "task start date"
                                    }
                                , E.text display
                                , case mbstart of
                                    Just start ->
                                        EI.button Common.buttonStyle
                                            { onPress = Just <| ChangeStart (Time.posixToMillis start)
                                            , label = E.text "ok"
                                            }

                                    Nothing ->
                                        EI.button Common.disabledButtonStyle
                                            { onPress = Nothing
                                            , label = E.text "ok"
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
                                E.row [ EE.onClick <| OnRowItemClick te.startdate End, igfont te ] [ E.text <| Util.showTime zone (Time.millisToPosix te.enddate) ]
                        in
                        if model.focus == Just ( te.startdate, End ) then
                            E.column [ E.spacing 8 ]
                                [ row
                                , EI.text [ E.width E.fill ]
                                    { onChange = FocusEndChanged
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
              , view =
                    \te ->
                        let
                            row =
                                E.row [ EE.onClick <| OnRowItemClick te.startdate Duration, igfont te ]
                                    [ E.text <| R.round 2 (toFloat (te.enddate - te.startdate) / (1000.0 * 60.0 * 60.0)) ]
                        in
                        if model.focus == Just ( te.startdate, Duration ) then
                            E.column [ E.spacing 8, E.width E.shrink ]
                                [ row
                                , EI.text [ E.width E.shrink ]
                                    { onChange = FocusDurationChanged
                                    , text = model.focusduration
                                    , placeholder = Nothing
                                    , label = EI.labelHidden "task duration"
                                    }
                                ]

                        else
                            row
              }
            ]
        }
    , E.row [ E.width E.fill, E.spacing 8 ]
        [ E.text "team unpaid hours: "
        , E.text <| R.round 2 <| teamhours - teampay
        ]
    , E.row [ E.width E.fill, E.spacing 8 ]
        [ E.text "my unpaid hours: "
        , E.text <| R.round 2 <| myhours - mypay
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


type Entry
    = TimeDay (TDict UserId Int Int)
    | PayEntry EditPayEntry


payview : Data.LoginData -> Util.Size -> Time.Zone -> Model -> List (Element Msg)
payview ld size zone model =
    let
        paytotes =
            model.payentries |> Dict.values |> TR.payTotes

        timetotes =
            model.timeentries |> Dict.values |> TR.timeTotes

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

        tmpd =
            TR.teamMillisPerDay (Dict.values model.timeentries)

        anychecked =
            Dict.foldl (\_ pe c -> c || pe.checked) False model.payentries
    in
    [ if anychecked then
        E.row [ E.spacing 8 ]
            [ E.text "checked items: "
            , EI.button Common.buttonStyle
                { onPress = Just <| DeletePayChecked
                , label = E.text "delete"
                }
            ]

      else
        E.none
    , E.table [ E.spacing 8, E.width E.fill ]
        { data =
            Dict.toList <|
                Dict.union (Dict.map (\i v -> TimeDay v) tmpd) (Dict.map (\i v -> PayEntry v) model.payentries)
        , columns =
            { header =
                EI.checkbox [ E.width E.shrink ]
                    { onChange = CheckPayAll
                    , icon = EI.defaultCheckbox
                    , checked =
                        Dict.foldl
                            (\_ pe ac ->
                                if pe.user == ld.userid then
                                    ac && pe.checked

                                else
                                    ac
                            )
                            True
                            model.payentries
                    , label = EI.labelHidden "check all"
                    }
            , width = E.shrink
            , view =
                \( date, entry ) ->
                    case entry of
                        PayEntry pe ->
                            EI.checkbox [ E.width E.shrink ]
                                { onChange = CheckPayItem pe.paymentdate
                                , icon = EI.defaultCheckbox
                                , checked = pe.checked
                                , label = EI.labelHidden "check item"
                                }

                        TimeDay _ ->
                            E.none
            }
                :: { header = E.text "date"
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
                                                let
                                                    row =
                                                        E.row
                                                            [ EE.onClick <| OnRowItemClick date PaymentDate
                                                            , EF.bold
                                                            ]
                                                            [ E.text <|
                                                                String.fromInt (Calendar.getYear cdate)
                                                                    ++ "/"
                                                                    ++ (cdate |> Calendar.getMonth |> Calendar.monthToInt |> String.fromInt)
                                                                    ++ "/"
                                                                    ++ String.fromInt
                                                                        (Calendar.getDay cdate)
                                                            ]
                                                in
                                                if model.focus == Just ( pe.paymentdate, PaymentDate ) then
                                                    let
                                                        ( display, mbstart ) =
                                                            case Util.parseTime zone model.focuspaydate of
                                                                Err e ->
                                                                    ( Util.deadEndsToString e, Nothing )

                                                                Ok Nothing ->
                                                                    ( "invalid", Nothing )

                                                                Ok (Just dt) ->
                                                                    ( Util.showTime zone dt, Just dt )
                                                    in
                                                    E.column [ E.spacing 8 ]
                                                        [ row
                                                        , EI.text [ E.width E.fill ]
                                                            { onChange = FocusPayDateChanged
                                                            , text = model.focuspaydate
                                                            , placeholder = Nothing
                                                            , label = EI.labelHidden "payment date"
                                                            }
                                                        , E.text display
                                                        , case mbstart of
                                                            Just start ->
                                                                EI.button Common.buttonStyle
                                                                    { onPress = Just <| ChangePayDate (Time.posixToMillis start)
                                                                    , label = E.text "ok"
                                                                    }

                                                            Nothing ->
                                                                EI.button Common.disabledButtonStyle
                                                                    { onPress = Nothing
                                                                    , label = E.text "ok"
                                                                    }
                                                        ]

                                                else
                                                    row
                                           )
                   }
                :: (model.members
                        |> List.map
                            (\member ->
                                { header = E.text member.name
                                , width = E.fill
                                , view =
                                    \( date, e ) ->
                                        case e of
                                            TimeDay ums ->
                                                case TDict.get member.id ums of
                                                    Just millis ->
                                                        if millis > 0 then
                                                            E.row [ EE.onClick <| OnRowItemClick date PaymentAmount ]
                                                                [ E.text <| R.round 2 (toFloat millis / (1000.0 * 60.0 * 60.0))
                                                                ]

                                                        else
                                                            E.none

                                                    Nothing ->
                                                        E.none

                                            PayEntry epe ->
                                                if epe.user == member.id then
                                                    let
                                                        s =
                                                            R.round 2 (toFloat epe.duration / (1000.0 * 60.0 * 60.0))

                                                        p =
                                                            E.el [ EF.bold ] <| E.text <| s ++ " pmt"
                                                    in
                                                    if model.focus == Just ( date, PaymentAmount ) then
                                                        E.column []
                                                            [ E.row [ EE.onClick <| OnRowItemClick date PaymentAmount ]
                                                                [ p
                                                                ]
                                                            , EI.text [ E.width E.fill ]
                                                                { onChange = FocusPayChanged
                                                                , text = model.focuspay
                                                                , placeholder = Nothing
                                                                , label = EI.labelHidden "payment"
                                                                }
                                                            ]

                                                    else
                                                        E.row [ EE.onClick <| OnRowItemClick date PaymentAmount ]
                                                            [ p
                                                            ]

                                                else
                                                    E.none
                                }
                            )
                   )
        }
    , E.table [ E.paddingXY 0 10, E.spacing 8, E.width E.fill ]
        { data = [ ( "total time", timetotes ), ( "total pay", paytotes ), ( "total unpaid", unpaidtotes ) ]
        , columns =
            -- dummy checkboxes for alignment.  alpha 0 hides them.
            { header =
                EI.checkbox [ E.width E.shrink, E.alpha 0.0 ]
                    { onChange = \_ -> Noop
                    , icon = EI.defaultCheckbox
                    , checked = False
                    , label = EI.labelHidden "alignment checkbox"
                    }
            , width = E.shrink
            , view =
                \_ ->
                    EI.checkbox [ E.width E.shrink, E.alpha 0.0 ]
                        { onChange = \_ -> Noop
                        , icon = EI.defaultCheckbox
                        , checked = False
                        , label = EI.labelHidden "alignment checkbox"
                        }
            }
                :: { header = E.text "totals"
                   , width = E.fill
                   , view =
                        \( title, _ ) ->
                            E.text title
                   }
                :: (model.members
                        |> List.map
                            (\member ->
                                { header = E.text member.name
                                , width = E.fill
                                , view =
                                    \( _, totes ) ->
                                        TDict.get member.id totes
                                            |> Maybe.map (\t -> E.text <| R.round 2 <| TR.millisToHours t)
                                            |> Maybe.withDefault E.none
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
                    model.members |> List.map (\m -> ( m.id, m )) |> TDict.insertList TR.emptyUmDict
            in
            E.table [ E.spacing 8, E.width E.fill ]
                { data = dist |> TDict.toList
                , columns =
                    [ { header = E.text "User"
                      , width = E.shrink
                      , view =
                            \( id, _ ) ->
                                case TDict.get id md of
                                    Just m ->
                                        E.el [ E.centerY ] <| E.text m.name

                                    Nothing ->
                                        E.none
                      }
                    , { header = E.text "Hours"
                      , width = E.shrink
                      , view =
                            \( user, hours ) ->
                                E.row [ E.spacing 8 ]
                                    [ EI.text []
                                        { onChange = OnPaymentChanged user
                                        , text = hours
                                        , placeholder = Nothing
                                        , label = EI.labelHidden "member hours"
                                        }
                                    , case
                                        hours
                                            |> String.toFloat
                                            |> Maybe.map (\f -> f * 60 * 60 * 1000 |> round)
                                      of
                                        Just millis ->
                                            EI.button Common.buttonStyle
                                                { onPress = Just <| AddPaymentPress user millis, label = E.text "add" }

                                        Nothing ->
                                            E.none
                                    ]
                      }
                    ]
                }

        Nothing ->
            E.none
    ]


update : Msg -> Model -> Data.LoginData -> Time.Zone -> ( Model, Command )
update msg model ld zone =
    case msg of
        DescriptionChanged t ->
            ( { model | description = t }, None )

        SavePress ->
            ( model, Save (toSaveProjectTime model) )

        RevertPress ->
            ( { model
                | timeentries = model.initialtimeentries
                , payentries = model.initialpayentries
              }
            , None
            )

        EditPress ->
            ( model, Edit )

        ImportPress ->
            ( model, GetCsv )

        CsvString str ->
            case
                Csv.parse str
                    |> Result.mapError
                        (List.map
                            Util.deadEndToString
                        )
                    |> Result.andThen
                        (\csv ->
                            csvToItems zone ld.userid csv
                        )
                    |> Result.mapError
                        (\strs ->
                            List.intersperse "\n" strs
                                |> String.concat
                        )
            of
                Ok el ->
                    ( { model
                        | timeentries =
                            el
                                |> List.map (\e -> ( e.startdate, e ))
                                |> List.foldl (\( k, v ) d -> Dict.insert k v d) model.timeentries
                      }
                    , None
                    )

                Err e ->
                    ( model, ShowError e )

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
                        , ignore = False
                        , checked = False
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

        DeleteClonk startdate ->
            ( { model
                | timeentries =
                    Dict.remove startdate model.timeentries
              }
            , None
            )

        DeletePay startdate ->
            ( { model
                | payentries =
                    Dict.remove startdate model.payentries
              }
            , None
            )

        OnRowItemClick i fc ->
            if model.focus == Just ( i, fc ) then
                ( { model | focus = Nothing }, None )

            else
                case model.viewmode of
                    Clonk ->
                        case Dict.get i model.timeentries of
                            Just te ->
                                ( { model
                                    | focus = Just ( i, fc )
                                    , focusdescription = te.description
                                    , focusstart = Util.showTime zone (Time.millisToPosix te.startdate)
                                    , focusend = Util.showTime zone (Time.millisToPosix te.enddate)
                                    , focusduration = R.round 2 (toFloat (te.enddate - te.startdate) / (1000.0 * 60.0 * 60.0))
                                  }
                                , None
                                )

                            Nothing ->
                                ( model, None )

                    Payment ->
                        case Dict.get i model.payentries of
                            Just pe ->
                                ( { model
                                    | focus = Just ( i, fc )
                                    , focusdescription = ""
                                    , focusstart = ""
                                    , focusend = ""
                                    , focusduration = ""
                                    , focuspay = R.round 2 (toFloat pe.duration / (1000.0 * 60.0 * 60.0))
                                    , focuspaydate = Util.showTime zone (Time.millisToPosix pe.paymentdate)
                                  }
                                , None
                                )

                            Nothing ->
                                ( model, None )

        FocusDescriptionChanged text ->
            case model.focus of
                Just ( startdate, _ ) ->
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

        FocusStartChanged text ->
            ( { model | focusstart = text }, None )

        ChangeStart newtime ->
            case model.focus of
                Just ( startdate, _ ) ->
                    case Dict.get startdate model.timeentries of
                        Just te ->
                            ( { model
                                | timeentries =
                                    Dict.insert newtime { te | startdate = newtime } model.timeentries
                                        |> Dict.remove startdate
                                , focus = Nothing
                                , focusstart = ""
                              }
                            , None
                            )

                        Nothing ->
                            ( model, None )

                Nothing ->
                    ( model, None )

        FocusPayDateChanged text ->
            ( { model | focuspaydate = text }, None )

        ChangePayDate newtime ->
            case model.focus of
                Just ( paymentdate, _ ) ->
                    case Dict.get paymentdate model.payentries of
                        Just pe ->
                            ( { model
                                | payentries =
                                    Dict.insert newtime { pe | paymentdate = newtime } model.payentries
                                        |> Dict.remove paymentdate
                                , focus = Nothing
                                , focuspaydate = ""
                              }
                            , None
                            )

                        Nothing ->
                            ( model, None )

                Nothing ->
                    ( model, None )

        FocusEndChanged text ->
            case ( model.focus, Util.parseTime zone text ) of
                ( Just ( startdate, _ ), Ok (Just time) ) ->
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

        FocusDurationChanged text ->
            case model.focus of
                Just ( startdate, _ ) ->
                    case Dict.get startdate model.timeentries of
                        Just te ->
                            case String.toFloat text of
                                Just hours ->
                                    let
                                        newtime =
                                            te.startdate + (round <| hours * 60 * 60 * 1000)
                                    in
                                    ( { model
                                        | timeentries =
                                            Dict.insert startdate { te | enddate = newtime } model.timeentries
                                        , focusduration = text
                                      }
                                    , None
                                    )

                                Nothing ->
                                    ( { model | focusduration = text }, None )

                        Nothing ->
                            ( { model | focusduration = text }, None )

                _ ->
                    ( { model | focusduration = text }, None )

        FocusPayChanged s ->
            ( { model
                | focuspay = s
                , payentries =
                    case String.toFloat s of
                        Just f ->
                            model.focus
                                |> Maybe.map Tuple.first
                                |> Maybe.andThen (\r -> Dict.get r model.payentries)
                                |> Maybe.map (\pe -> Dict.insert pe.paymentdate { pe | duration = round <| f * 60 * 60 * 1000 } model.payentries)
                                |> Maybe.withDefault model.payentries

                        Nothing ->
                            model.payentries
              }
            , None
            )

        OnDistributionChanged text ->
            ( { model | distributionhours = text }, None )

        ClearDistribution ->
            ( { model | distributionhours = "", distribution = Nothing }, None )

        CalcDistribution ->
            case String.toFloat model.distributionhours of
                Just hours ->
                    let
                        -- total millis per day for each member.
                        utmpd =
                            TR.teamMillisPerDay (Dict.values model.timeentries)

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
                                    |> TDict.map (\_ i -> R.round 2 (toFloat i / (60 * 60 * 1000)))
                                )
                      }
                    , None
                    )

                Nothing ->
                    ( model, None )

        OnPaymentChanged member text ->
            case model.distribution of
                Just dist ->
                    ( { model
                        | distribution =
                            Just <|
                                TDict.insert member text dist
                      }
                    , None
                    )

                Nothing ->
                    ( model, None )

        AddPaymentPress member payment ->
            ( model, GetTime (AddPayment member payment) )

        AddPayment member payment paydate ->
            ( { model
                | payentries =
                    Dict.insert paydate
                        { id = Nothing
                        , user = member
                        , description = "payment"
                        , paymentdate = paydate
                        , duration = payment
                        , checked = False
                        }
                        model.payentries
              }
            , None
            )

        CheckAll c ->
            ( { model
                | timeentries =
                    Dict.map
                        (\_ te ->
                            if te.user == ld.userid then
                                { te | checked = c }

                            else
                                te
                        )
                        model.timeentries
              }
            , None
            )

        CheckItem sd c ->
            ( { model
                | timeentries =
                    model.timeentries
                        |> Dict.get sd
                        |> Maybe.map (\te -> Dict.insert sd { te | checked = c } model.timeentries)
                        |> Maybe.withDefault model.timeentries
              }
            , None
            )

        CheckPayAll c ->
            ( { model
                | payentries =
                    Dict.map
                        (\_ pe ->
                            { pe | checked = c }
                        )
                        model.payentries
              }
            , None
            )

        CheckPayItem sd c ->
            ( { model
                | payentries =
                    model.payentries
                        |> Dict.get sd
                        |> Maybe.map (\pe -> Dict.insert sd { pe | checked = c } model.payentries)
                        |> Maybe.withDefault model.payentries
              }
            , None
            )

        DeletePayChecked ->
            ( { model
                | payentries = Dict.filter (\_ pe -> not pe.checked) model.payentries
              }
            , None
            )

        DeleteChecked ->
            ( { model
                | timeentries = Dict.filter (\_ te -> not te.checked) model.timeentries
              }
            , None
            )

        IgnoreChecked ->
            let
                anyunignored =
                    Dict.foldl
                        (\_ te any ->
                            if te.checked then
                                any || not te.ignore

                            else
                                any
                        )
                        False
                        model.timeentries

                igftn =
                    if anyunignored then
                        always True

                    else
                        not
            in
            ( { model
                | timeentries =
                    Dict.map
                        (\_ te ->
                            if te.checked then
                                { te | ignore = igftn te.ignore }

                            else
                                te
                        )
                        model.timeentries
              }
            , None
            )

        DonePress ->
            ( model, Done )

        Noop ->
            ( model, None )


csvToItems : Time.Zone -> UserId -> Csv.Csv -> Result (List String) (List EditTimeEntry)
csvToItems zone user csv =
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
