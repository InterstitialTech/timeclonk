module ProjectTime exposing (..)

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
    = DescriptionChanged String
    | SavePress
    | RevertPress
    | DonePress
    | EditPress
    | SettingsPress
    | ImportPress
    | ExportAll
    | CsvString String
    | ClonkInPress
    | ClonkOutPress
    | ClonkInTime Int
    | ClonkOutTime Int
    | NowEnd Int
    | NowEndTime Int Int
    | ClearEnd Int
    | DeleteClonk Int
    | DeletePay Int
    | AllocDescriptionChanged Int String
    | EteDescriptionChanged Int String
    | FocusCancel
    | ChangeEnd Int Int
    | FocusDescriptionChanged String
    | ChangeDescription
    | FocusStartChanged String
    | FocusEndChanged String
    | FocusDurationChanged String
    | FocusPayChanged String
    | ChangePay Float
    | FocusPayDateChanged String
    | FocusAllocationChanged String
    | ChangeAllocation Float
    | NewAllocDescriptionChanged String
    | NewAllocHoursChanged String
    | NewAllocCurrencyChanged String
    | NewPaymentHoursChanged String
    | NewPaymentCurrencyChanged String
    | SelectPaymentUser
    | AddAllocationPress
    | AddAllocation Int
    | ChangeStart Int
    | ChangePayDate Int
    | ToggleNewAlloc
    | ToggleNewPayment
    | ChangeAllocationDate Int
    | ChangePaymentDate Int
    | SetViewMode ViewMode
    | OnRowItemClick Int FocusColumn
    | OnDistributionChanged String
    | OnDistCurrencyChanged String
    | ClearDistribution
    | CalcDistribution
    | OnPaymentChanged UserId String
    | AddPaymentPress UserId Int
    | AddPayment UserId Int Int
    | CheckAll Bool
    | CheckItem Int Bool
    | CheckPayAll Bool
    | CheckAllocAll Bool
    | CheckPayItem Int Bool
    | CheckAllocationItem Int Bool
    | DeleteChecked
    | DeletePayChecked
    | DeleteAllocationChecked
    | IgnoreChecked
    | TeForward
    | TeBack Int
    | TeToStart
    | TeToEnd Int
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
    = Clonks
    | Team
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
    , description : String
    , timeentries : TTotaler
    , initialtimeentries : Dict Int EditTimeEntry
    , tepaginator : P.Model Msg
    , teamentries : TTotaler
    , teampaginator : P.Model Msg
    , payentries : Dict Int EditPayEntry
    , initialpayentries : Dict Int EditPayEntry
    , pepaginator : P.Model Msg
    , allocations : Dict Int EditAllocation
    , initialallocations : Dict Int EditAllocation
    , apaginator : P.Model Msg
    , focusstart : String
    , focusend : String
    , focusduration : String
    , focusdescription : String
    , focus : Maybe ( Int, FocusColumn )
    , focuspay : String
    , focuspaydate : String
    , distributionhours : String
    , distributioncurrency : String
    , distribution : Maybe (TDict UserId Int String)
    , dpaginator : P.Model Msg
    , shownewalloc : Bool
    , shownewpayment : Bool
    , allocdescription : String
    , allochours : String
    , alloccurrency : String
    , paymenthours : String
    , paymentcurrency : String
    , paymentuser : Maybe UserId
    , viewmode : ViewMode
    , saveonclonk : Bool
    , clonkOutDisplay : Maybe Time.Posix
    }


type Command
    = Save Data.SaveProjectTime
    | Edit
    | Done
    | GetTime (Int -> Msg)
    | GetCsv
    | SaveCsv String String
    | Settings
    | ShowError String
    | SelectMember (List Data.User)
    | None


headerStyle : List (E.Attribute msg)
headerStyle =
    [ EF.bold ]


onClockTick : Time.Posix -> Model -> Model
onClockTick time model =
    { model
        | clonkOutDisplay = Just time
    }


onMemberSelected : UserId -> Model -> Model
onMemberSelected user model =
    case model.focus of
        Just ( date, PaymentUser ) ->
            { model
                | payentries =
                    model.payentries
                        |> Dict.get date
                        |> Maybe.map (\pe -> Dict.insert date { pe | user = user } model.payentries)
                        |> Maybe.withDefault model.payentries
            }

        _ ->
            { model | paymentuser = Just user }


showViewMode : ViewMode -> String
showViewMode mode =
    case mode of
        Clonks ->
            "clonks"

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
        "clonks" ->
            Just Clonks

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
            getTes model.timeentries
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
            Dict.diff model.initialtimeentries (getTes model.timeentries)
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

        saveallocations =
            model.allocations
                |> Dict.values
                |> List.foldl
                    (\pe saves ->
                        case Dict.get pe.allocationdate model.initialallocations of
                            Just ipe ->
                                if pe /= ipe then
                                    pe :: saves

                                else
                                    saves

                            Nothing ->
                                pe :: saves
                    )
                    []
                |> List.map (toSaveAllocation model)

        deleteallocations =
            Dict.diff model.initialallocations model.allocations
                |> Dict.values
                |> List.filterMap .id
                |> TSet.insertList emptyAllocationIdSet
    in
    { project = model.project.id
    , savetimeentries = savetimeentries
    , deletetimeentries =
        List.foldl
            -- remove update ids from the delete list.
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
    , deletepayentries =
        List.foldl
            -- remove update ids from the delete list.
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
    , saveallocations = saveallocations
    , deleteallocations =
        List.foldl
            -- remove update ids from the delete list.
            (\ste dte ->
                case ste.id of
                    Just id ->
                        TSet.remove id dte

                    Nothing ->
                        dte
            )
            deleteallocations
            saveallocations
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


toEditAllocation : Data.Allocation -> EditAllocation
toEditAllocation e =
    { id = Just e.id
    , description = e.description
    , allocationdate = e.allocationdate
    , duration = e.duration
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


toSaveAllocation : Model -> EditAllocation -> Data.SaveAllocation
toSaveAllocation model e =
    { id = e.id
    , project = model.project.id
    , description = e.description
    , allocationdate = e.allocationdate
    , duration = e.duration
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


init : Time.Zone -> Data.LoginData -> Data.ProjectTime -> Bool -> Int -> String -> Model
init zone ld pt saveonclonk pageincrement mode =
    let
        ietes =
            toEteDict pt.timeentries

        iepes =
            toEpeDict pt.payentries

        ieas =
            toEaDict pt.allocations

        description =
            ietes |> Dict.toList |> List.filter (\( _, e ) -> e.user == ld.userid) |> List.reverse |> List.head |> Maybe.map (\( _, ete ) -> ete.description) |> Maybe.withDefault ""
    in
    { project = pt.project
    , members = pt.members
    , membernames = pt.members |> List.map (\m -> ( Data.getUserIdVal m.id, m.name )) |> Dict.fromList
    , description = description
    , timeentries = mkTToteler ietes (\te -> te.user == ld.userid) zone
    , initialtimeentries = ietes
    , tepaginator = P.init TeForward TeBack TeToStart TeToEnd P.End pageincrement
    , teamentries = mkTToteler ietes (always True) zone
    , teampaginator = P.init TeamForward TeamBack TeamToStart TeamToEnd P.End pageincrement
    , payentries = iepes
    , initialpayentries = iepes
    , pepaginator = P.init PeForward PeBack PeToStart PeToEnd P.End pageincrement
    , allocations = ieas
    , initialallocations = ieas
    , apaginator = P.init AForward ABack AToStart AToEnd P.End pageincrement
    , viewmode = readViewMode mode |> Maybe.withDefault Clonks
    , focusstart = ""
    , focusend = ""
    , focusduration = ""
    , focusdescription = ""
    , focus = Nothing
    , focuspay = ""
    , focuspaydate = ""
    , distributionhours = ""
    , distributioncurrency = ""
    , distribution = Nothing
    , dpaginator = P.init DForward DBack DToStart DToEnd P.End pageincrement
    , shownewalloc = False
    , shownewpayment = False
    , allocdescription = ""
    , allochours = ""
    , alloccurrency = ""
    , paymenthours = ""
    , paymentcurrency = ""
    , paymentuser = Nothing
    , saveonclonk = saveonclonk
    , clonkOutDisplay = Nothing
    }


setPageIncrement : Int -> Model -> Model
setPageIncrement pageincrement model =
    let
        tp =
            model.tepaginator

        pp =
            model.pepaginator

        ap =
            model.apaginator

        dp =
            model.dpaginator
    in
    { model
        | tepaginator = { tp | pageincrement = pageincrement }
        , pepaginator = { pp | pageincrement = pageincrement }
        , apaginator = { ap | pageincrement = pageincrement }
        , dpaginator = { dp | pageincrement = pageincrement }
    }


onProjectTime : Time.Zone -> Data.LoginData -> Data.ProjectTime -> Model -> Model
onProjectTime zone ld pt model =
    let
        nm =
            init zone ld pt model.saveonclonk model.tepaginator.pageincrement (showViewMode model.viewmode)
    in
    { nm
        | tepaginator = model.tepaginator
        , pepaginator = model.pepaginator
        , apaginator = model.apaginator
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
        [ vbt Clonks "clonks"
        , vbt Team "team"
        , vbt Payments "payments"
        , vbt Allocations "allocations"
        , vbt Distributions "distributions"
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
            , E.row [ E.spacing TC.defaultSpacing ] [ E.text "project:", E.el [ EF.bold ] <| E.text model.project.name ]
            , E.row [ E.spacing TC.defaultSpacing ] <|
                [ EI.button Common.buttonStyle { onPress = Just DonePress, label = E.text "<-" }
                , EI.button Common.buttonStyle { onPress = Just EditPress, label = E.text "edit project" }
                , EI.button Common.buttonStyle { onPress = Just ImportPress, label = E.text "import" }
                , EI.button Common.buttonStyle { onPress = Just ExportAll, label = E.text "export" }
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
                        Clonks ->
                            clonkview ld size zone isdirty model

                        Team ->
                            teamview ld size zone isdirty model

                        Payments ->
                            payview ld size zone model

                        Allocations ->
                            allocationview ld size zone model

                        Distributions ->
                            distributionview ld size zone model
                   )


cellEditStyle =
    [ E.spacing TC.defaultSpacing, EBk.color TC.darkGrey, E.padding TC.defaultSpacing ]


dateTimeWidth =
    200


clonkview : Data.LoginData -> Util.Size -> Time.Zone -> Bool -> Model -> List (Element Msg)
clonkview ld size zone isdirty model =
    let
        ttotes =
            getTotes model.timeentries

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

        anychecked =
            Dict.foldl (\_ te c -> c || te.checked) False (getTes model.timeentries)

        cmillis =
            \te ->
                if Just te.startdate == ttotes.lasttime && te.startdate == te.enddate then
                    case model.clonkOutDisplay of
                        Just time ->
                            Just <| Time.posixToMillis time - te.startdate

                        Nothing ->
                            Nothing

                else
                    Nothing
    in
    [ E.row
        [ E.spacing TC.defaultSpacing
        , E.transparent (not anychecked)
        , if anychecked then
            E.height E.shrink

          else
            E.height <| E.px 0
        ]
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
    , P.view ttotes.mtecount model.tepaginator
    , E.table [ E.spacing TC.defaultSpacing, E.width E.fill ]
        { data =
            P.filter model.tepaginator ttotes.mytimeentries
        , columns =
            [ { header =
                    EI.checkbox [ E.width E.shrink, E.centerY ]
                        { onChange = CheckAll
                        , icon = TC.checkboxIcon
                        , checked =
                            Dict.foldl
                                (\_ te ac ->
                                    if te.user == ld.userid then
                                        ac && te.checked

                                    else
                                        ac
                                )
                                True
                                (getTes model.timeentries)
                        , label = EI.labelHidden "check all"
                        }
              , width = E.shrink
              , view =
                    \te ->
                        EI.checkbox [ E.width E.shrink, E.centerY ]
                            { onChange = CheckItem te.startdate
                            , icon = TC.checkboxIcon
                            , checked = te.checked
                            , label = EI.labelHidden "check item"
                            }
              }
            , { header = E.el headerStyle <| E.text "task"
              , width = E.fill
              , view =
                    \te ->
                        let
                            row =
                                E.row
                                    [ EE.onClick <| OnRowItemClick te.startdate Description
                                    , igfont te
                                    , E.width E.fill
                                    ]
                                    [ E.text te.description ]
                        in
                        if model.focus == Just ( te.startdate, Description ) then
                            E.column
                                cellEditStyle
                                [ row
                                , EI.text [ E.width E.fill ]
                                    { onChange = FocusDescriptionChanged
                                    , text = model.focusdescription
                                    , placeholder = Nothing
                                    , label = EI.labelHidden "task description"
                                    }
                                , E.row [ E.width E.fill ]
                                    [ EI.button Common.buttonStyle
                                        { onPress = Just <| ChangeDescription
                                        , label = E.text "ok"
                                        }
                                    , EI.button (E.alignRight :: Common.buttonStyle)
                                        { onPress = Just FocusCancel
                                        , label = E.text "cancel"
                                        }
                                    ]
                                ]

                        else
                            row
              }
            , { header = E.el headerStyle <| E.text "start"
              , width = E.fill
              , view =
                    \te ->
                        let
                            row =
                                E.row
                                    [ EE.onClick <| OnRowItemClick te.startdate Start
                                    , igfont te
                                    , E.width E.fill
                                    ]
                                    [ E.text <| Util.showDateTime zone (Time.millisToPosix te.startdate) ]
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
                                            ( Util.showDateTime zone dt, Just dt )
                            in
                            E.column cellEditStyle
                                [ row
                                , EI.text [ E.width <| E.px dateTimeWidth ]
                                    { onChange = FocusStartChanged
                                    , text = model.focusstart
                                    , placeholder = Nothing
                                    , label = EI.labelHidden "task start date"
                                    }
                                , E.text display
                                , E.row [ E.width E.fill ]
                                    [ case mbstart of
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
                                    , EI.button (E.alignRight :: Common.buttonStyle)
                                        { onPress = Just FocusCancel
                                        , label = E.text "cancel"
                                        }
                                    ]
                                ]

                        else
                            row
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

                            row =
                                E.row
                                    [ EE.onClick <| OnRowItemClick te.startdate End
                                    , igfont te
                                    , E.width E.fill
                                    ]
                                    [ E.text <| endtext te.enddate
                                    ]
                        in
                        if model.focus == Just ( te.startdate, End ) then
                            let
                                ( display, mbend ) =
                                    case Util.parseTime zone model.focusend of
                                        Err e ->
                                            ( Util.deadEndsToString e, Nothing )

                                        Ok Nothing ->
                                            ( "invalid", Nothing )

                                        Ok (Just dt) ->
                                            ( endtext (Time.posixToMillis dt), Just dt )
                            in
                            E.column cellEditStyle
                                [ row
                                , E.row [ E.width E.fill ]
                                    [ if Just te.startdate == ttotes.lasttime then
                                        EI.button Common.buttonStyle
                                            { onPress = Just <| ChangeEnd te.startdate te.startdate
                                            , label = E.text "clonk in"
                                            }

                                      else
                                        EI.button Common.buttonStyle
                                            { onPress =
                                                Just <|
                                                    FocusEndChanged
                                                        (Util.showDateTime zone
                                                            (Time.millisToPosix te.startdate)
                                                        )
                                            , label =
                                                E.text
                                                    "copy start"
                                            }
                                    ]
                                , EI.text [ E.width <| E.px dateTimeWidth ]
                                    { onChange = FocusEndChanged
                                    , text = model.focusend
                                    , placeholder = Nothing
                                    , label = EI.labelHidden "task end date"
                                    }
                                , E.text display
                                , E.row [ E.width E.fill ]
                                    [ case mbend of
                                        Just end ->
                                            EI.button Common.buttonStyle
                                                { onPress = Just <| ChangeEnd te.startdate (Time.posixToMillis end)
                                                , label = E.text "ok"
                                                }

                                        Nothing ->
                                            EI.button Common.disabledButtonStyle
                                                { onPress = Nothing
                                                , label = E.text "ok"
                                                }
                                    , EI.button (E.alignRight :: Common.buttonStyle)
                                        { onPress = Just FocusCancel
                                        , label = E.text "cancel"
                                        }
                                    ]
                                ]

                        else if te.startdate == te.enddate then
                            let
                                reg_row =
                                    E.row
                                        [ EE.onClick <| OnRowItemClick te.startdate End
                                        , igfont te
                                        , E.height E.fill
                                        ]
                                        [ E.none ]
                            in
                            if Just te.startdate == ttotes.lasttime then
                                case model.clonkOutDisplay of
                                    Just time ->
                                        E.row
                                            [ EE.onClick <| OnRowItemClick te.startdate End
                                            , igfont te
                                            , E.height E.fill
                                            ]
                                            [ E.el [ EF.color TC.darkGreen ] <|
                                                E.text <|
                                                    if
                                                        Util.sameDay zone
                                                            (Time.millisToPosix te.startdate)
                                                            time
                                                    then
                                                        Util.showTime zone time

                                                    else
                                                        Util.showDateTime zone time
                                            ]

                                    Nothing ->
                                        reg_row

                            else
                                reg_row

                        else
                            row
              }
            , { header = E.el headerStyle <| E.text "duration"
              , width = E.shrink
              , view =
                    \te ->
                        let
                            row =
                                E.row
                                    [ EE.onClick <| OnRowItemClick te.startdate Duration
                                    , igfont te
                                    , E.width E.fill
                                    ]
                                    [ E.text <| millisAsHours (te.enddate - te.startdate) ]
                        in
                        if model.focus == Just ( te.startdate, Duration ) then
                            E.column cellEditStyle
                                [ row
                                , EI.text [ E.width E.shrink ]
                                    { onChange = FocusDurationChanged
                                    , text = model.focusduration
                                    , placeholder = Nothing
                                    , label = EI.labelHidden "task duration"
                                    }
                                , E.row [ E.width E.fill ]
                                    [ case String.toFloat model.focusduration of
                                        Just hours ->
                                            let
                                                newtime =
                                                    te.startdate + (round <| hours * 60 * 60 * 1000)
                                            in
                                            EI.button Common.buttonStyle
                                                { onPress = Just <| ChangeEnd te.startdate newtime
                                                , label = E.text "ok"
                                                }

                                        Nothing ->
                                            EI.button Common.disabledButtonStyle
                                                { onPress = Nothing
                                                , label = E.text "ok"
                                                }
                                    , EI.button (E.alignRight :: Common.buttonStyle)
                                        { onPress = Just FocusCancel
                                        , label = E.text "cancel"
                                        }
                                    ]
                                ]

                        else
                            case cmillis te of
                                Just time ->
                                    E.row
                                        [ EE.onClick <| OnRowItemClick te.startdate Duration
                                        , igfont te
                                        , E.width E.fill
                                        ]
                                        [ E.el [ EF.color TC.darkGreen ] <|
                                            E.text <|
                                                millisAsHours time
                                        ]

                                Nothing ->
                                    row
              }
            , { header = E.el headerStyle <| E.text "daily"
              , width = E.shrink
              , view =
                    \te ->
                        let
                            mbcm =
                                cmillis te

                            cm =
                                mbcm |> Maybe.withDefault 0
                        in
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
                                        (case mbcm of
                                            Just _ ->
                                                [ EF.color TC.darkGreen ]

                                            Nothing ->
                                                []
                                        )
                                    <|
                                        E.text (millisAsHours (cm + millis))

                                Nothing ->
                                    E.none

                        else
                            E.none
              }
            , { header = E.el headerStyle <| E.text "weekly"
              , width = E.shrink
              , view =
                    \te ->
                        let
                            mbcm =
                                cmillis te

                            cm =
                                mbcm |> Maybe.withDefault 0
                        in
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
                                        (case mbcm of
                                            Just _ ->
                                                [ EF.color TC.darkGreen ]

                                            Nothing ->
                                                []
                                        )
                                    <|
                                        E.text (millisAsHours (cm + millis))

                                Nothing ->
                                    E.none

                        else
                            E.none
              }
            ]
        }
    , if isdirty then
        E.row [ E.spacing TC.defaultSpacing ]
            [ EI.button Common.buttonStyle { onPress = Just RevertPress, label = E.text "revert" }
            , EI.button
                (Common.buttonStyle ++ [ EBk.color TC.darkYellow ])
                { onPress = Just SavePress, label = E.text "save" }
            ]

      else
        E.none
    , E.table [ E.spacing TC.defaultSpacing, E.width E.fill ]
        { data =
            [ ( "team unpaid hours: "
              , R.round 2 <| ttotes.teamhours - teampay
              )
            , ( "team allocated hours: "
              , R.round 2 <| teamalloc - ttotes.teamhours
              )
            , ( "my unpaid hours: "
              , R.round 2 <| ttotes.myhours - mypay
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
    , let
        hasendtime =
            ttotes.lasttime
                |> Maybe.andThen
                    (\t ->
                        Dict.get t (getTes model.timeentries)
                    )
                |> Maybe.map
                    (\te ->
                        te.startdate /= te.enddate
                    )
      in
      case hasendtime of
        Just False ->
            E.row [ E.width E.fill, E.spacing TC.defaultSpacing ]
                [ EI.text (E.width E.fill :: Common.disabledTextEditStyle)
                    { onChange = always Noop
                    , text = model.description
                    , placeholder = Nothing
                    , label = EI.labelLeft [] <| E.text "Current Task:"
                    }
                , EI.button Common.buttonStyle { onPress = Just ClonkOutPress, label = E.text "Clonk Out" }
                ]

        _ ->
            E.row [ E.width E.fill, E.spacing TC.defaultSpacing ]
                [ EI.text [ E.width E.fill ]
                    { onChange = DescriptionChanged
                    , text = model.description
                    , placeholder = Nothing
                    , label = EI.labelLeft [] <| E.text "Current Task:"
                    }
                , EI.button Common.buttonStyle { onPress = Just ClonkInPress, label = E.text "Clonk In" }
                ]
    ]


teamview : Data.LoginData -> Util.Size -> Time.Zone -> Bool -> Model -> List (Element Msg)
teamview ld size zone isdirty model =
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
                            [ EE.onClick <| OnRowItemClick te.startdate End
                            , igfont te
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
    , if isdirty then
        E.row [ E.spacing TC.defaultSpacing ]
            [ EI.button Common.buttonStyle { onPress = Just RevertPress, label = E.text "revert" }
            , EI.button
                (Common.buttonStyle ++ [ EBk.color TC.darkYellow ])
                { onPress = Just SavePress, label = E.text "save" }
            ]

      else
        E.none
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


distributionview : Data.LoginData -> Util.Size -> Time.Zone -> Model -> List (Element Msg)
distributionview ld size zone model =
    let
        paytotes =
            model.payentries |> Dict.values |> TR.payTotes

        timetotes =
            getTes model.timeentries |> Dict.values |> TR.timeTotes

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
            TR.teamMillisPerDay zone (Dict.values (getTes model.timeentries))

        anychecked =
            Dict.foldl (\_ pe c -> c || pe.checked) False model.payentries

        data =
            Dict.union (Dict.map (\i v -> TimeDay v) tmpd) <|
                Dict.union
                    (Dict.map (\i v -> PayEntry v) model.payentries)
                    (Dict.map (\i v -> Allocation v) model.allocations)
    in
    [ if anychecked then
        E.row [ E.spacing TC.defaultSpacing ]
            [ E.text "checked items: "
            , EI.button Common.buttonStyle
                { onPress = Just <| DeletePayChecked
                , label = E.text "delete"
                }
            ]

      else
        E.none
    , P.view (Dict.size data) model.dpaginator
    , E.table [ E.spacing TC.defaultSpacing, E.width E.fill ]
        { data =
            data
                |> Dict.toList
                |> P.filter model.dpaginator
        , columns =
            { header =
                EI.checkbox [ E.width E.shrink, E.centerY ]
                    { onChange = CheckPayAll
                    , icon = TC.checkboxIcon
                    , checked =
                        Dict.foldl
                            (\_ pe ac ->
                                ac && pe.checked
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
                            EI.checkbox [ E.width E.shrink, E.centerY ]
                                { onChange = CheckPayItem pe.paymentdate
                                , icon = TC.checkboxIcon
                                , checked = pe.checked
                                , label = EI.labelHidden "check item"
                                }

                        Allocation e ->
                            E.none

                        TimeDay _ ->
                            E.none
            }
                :: { header = E.el headerStyle <| E.text "date"
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
                                                                    ( Util.showDateTime zone dt, Just dt )
                                                    in
                                                    E.column cellEditStyle
                                                        [ row
                                                        , EI.text [ E.width <| E.px dateTimeWidth ]
                                                            { onChange = FocusPayDateChanged
                                                            , text = model.focuspaydate
                                                            , placeholder = Nothing
                                                            , label = EI.labelHidden "payment date"
                                                            }
                                                        , E.text display
                                                        , E.row [ E.width E.fill ]
                                                            [ case mbstart of
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
                                                            , EI.button (E.alignRight :: Common.buttonStyle)
                                                                { onPress = Just FocusCancel
                                                                , label = E.text "cancel"
                                                                }
                                                            ]
                                                        ]

                                                else
                                                    row
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
                                                            E.row [ EE.onClick <| OnRowItemClick date PaymentAmount ]
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
                                                    if model.focus == Just ( date, PaymentAmount ) then
                                                        E.column cellEditStyle
                                                            [ E.row [ EE.onClick <| OnRowItemClick date PaymentAmount ]
                                                                [ p
                                                                ]
                                                            , EI.text [ E.width E.fill ]
                                                                { onChange = FocusPayChanged
                                                                , text = model.focuspay
                                                                , placeholder = Nothing
                                                                , label = EI.labelHidden "payment"
                                                                }
                                                            , E.row [ E.width E.fill, E.spacing TC.defaultSpacing ]
                                                                [ case String.toFloat model.focuspay of
                                                                    Just amt ->
                                                                        EI.button Common.buttonStyle
                                                                            { onPress = Just <| ChangePay amt
                                                                            , label = E.text "ok"
                                                                            }

                                                                    Nothing ->
                                                                        EI.button Common.disabledButtonStyle
                                                                            { onPress = Nothing
                                                                            , label = E.text "ok"
                                                                            }
                                                                , EI.button (E.alignRight :: Common.buttonStyle)
                                                                    { onPress = Just FocusCancel
                                                                    , label = E.text "cancel"
                                                                    }
                                                                ]
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
                                EI.text []
                                    { onChange = OnPaymentChanged user
                                    , text = hours
                                    , placeholder = Nothing
                                    , label = EI.labelHidden "member hours"
                                    }
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
                    , { header = E.none
                      , width = E.shrink
                      , view =
                            \( user, hours ) ->
                                E.row [ E.spacing TC.defaultSpacing ]
                                    [ case
                                        hours
                                            |> String.toFloat
                                            |> Maybe.map (\f -> f * 60 * 60 * 1000 |> round)
                                      of
                                        Just millis ->
                                            EI.button Common.buttonStyle
                                                { onPress = Just <| AddPaymentPress user millis, label = E.text "add" }
                                                |> E.el [ E.centerY ]

                                        Nothing ->
                                            E.none
                                    ]
                      }
                    ]
                }

        Nothing ->
            E.none
    ]


allocationview : Data.LoginData -> Util.Size -> Time.Zone -> Model -> List (Element Msg)
allocationview ld size zone model =
    let
        paytote =
            model.payentries
                |> Dict.values
                |> List.foldl (\e t -> t + e.duration) 0

        timetote =
            getTotes model.timeentries
                |> .teammillis

        alloctote =
            model.allocations
                |> Dict.values
                |> List.foldl (\e t -> t + e.duration) 0

        anychecked =
            Dict.foldl (\_ e c -> c || e.checked) False model.allocations
    in
    [ if anychecked then
        E.row [ E.spacing TC.defaultSpacing ]
            [ E.text "checked items: "
            , EI.button Common.buttonStyle
                { onPress = Just <| DeleteAllocationChecked
                , label = E.text "delete"
                }
            ]

      else
        E.none
    , P.view (Dict.size model.allocations) model.apaginator
    , E.table [ E.spacing TC.defaultSpacing, E.width E.fill ]
        { data =
            Dict.toList model.allocations
                |> P.filter model.apaginator
        , columns =
            { header =
                EI.checkbox [ E.width E.shrink, E.centerY ]
                    { onChange = CheckAllocAll
                    , icon = TC.checkboxIcon
                    , checked =
                        Dict.foldl
                            (\_ pe ac ->
                                ac && pe.checked
                            )
                            True
                            model.allocations
                    , label = EI.labelHidden "check all"
                    }
            , width = E.shrink
            , view =
                \( date, e ) ->
                    EI.checkbox [ E.width E.shrink, E.centerY ]
                        { onChange = CheckAllocationItem e.allocationdate
                        , icon = TC.checkboxIcon
                        , checked = e.checked
                        , label = EI.labelHidden "check item"
                        }
            }
                :: { header = E.el headerStyle <| E.text "date"
                   , width = E.fill
                   , view =
                        \( date, a ) ->
                            date
                                |> Time.millisToPosix
                                |> Calendar.fromPosix
                                |> (\cdate ->
                                        let
                                            row =
                                                E.row
                                                    [ EE.onClick <| OnRowItemClick date PaymentDate
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
                                        if model.focus == Just ( a.allocationdate, PaymentDate ) then
                                            let
                                                ( display, mbstart ) =
                                                    case Util.parseTime zone model.focuspaydate of
                                                        Err e ->
                                                            ( Util.deadEndsToString e, Nothing )

                                                        Ok Nothing ->
                                                            ( "invalid", Nothing )

                                                        Ok (Just dt) ->
                                                            ( Util.showDateTime zone dt, Just dt )
                                            in
                                            E.column cellEditStyle
                                                [ row
                                                , EI.text [ E.width <| E.px dateTimeWidth ]
                                                    { onChange = FocusPayDateChanged
                                                    , text = model.focuspaydate
                                                    , placeholder = Nothing
                                                    , label = EI.labelHidden "payment date"
                                                    }
                                                , E.text display
                                                , E.row [ E.width E.fill, E.spacing TC.defaultSpacing ]
                                                    [ case mbstart of
                                                        Just start ->
                                                            EI.button Common.buttonStyle
                                                                { onPress = Just <| ChangeAllocationDate (Time.posixToMillis start)
                                                                , label = E.text "ok"
                                                                }

                                                        Nothing ->
                                                            EI.button Common.disabledButtonStyle
                                                                { onPress = Nothing
                                                                , label = E.text "ok"
                                                                }
                                                    , EI.button (E.alignRight :: Common.buttonStyle)
                                                        { onPress = Just FocusCancel
                                                        , label = E.text "cancel"
                                                        }
                                                    ]
                                                ]

                                        else
                                            row
                                   )
                   }
                :: { header = E.el headerStyle <| E.text "description"
                   , width = E.fill
                   , view =
                        \( date, a ) ->
                            if model.focus == Just ( date, Description ) then
                                E.column cellEditStyle
                                    [ E.row [ EE.onClick <| OnRowItemClick date Description ]
                                        [ E.text a.description
                                        ]
                                    , EI.text [ E.width E.fill ]
                                        { onChange = FocusDescriptionChanged
                                        , text = model.focusdescription
                                        , placeholder = Nothing
                                        , label = EI.labelHidden "allocation description"
                                        }
                                    , E.row [ E.width E.fill, E.spacing TC.defaultSpacing ]
                                        [ EI.button Common.buttonStyle
                                            { onPress = Just <| ChangeDescription
                                            , label = E.text "ok"
                                            }
                                        , EI.button (E.alignRight :: Common.buttonStyle)
                                            { onPress = Just FocusCancel
                                            , label = E.text "cancel"
                                            }
                                        ]
                                    ]

                            else
                                E.row [ EE.onClick <| OnRowItemClick date Description ]
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
                            if model.focus == Just ( date, PaymentAmount ) then
                                E.column cellEditStyle
                                    [ E.row [ EE.onClick <| OnRowItemClick date PaymentAmount ]
                                        [ p
                                        ]
                                    , EI.text [ E.width E.fill ]
                                        { onChange = FocusAllocationChanged
                                        , text = model.focuspay
                                        , placeholder = Nothing
                                        , label = EI.labelHidden "allocation"
                                        }
                                    , E.row [ E.width E.fill, E.spacing TC.defaultSpacing ]
                                        [ case String.toFloat model.focuspay of
                                            Just amt ->
                                                EI.button Common.buttonStyle
                                                    { onPress = Just <| ChangeAllocation amt
                                                    , label = E.text "ok"
                                                    }

                                            Nothing ->
                                                EI.button Common.disabledButtonStyle
                                                    { onPress = Nothing
                                                    , label = E.text "ok"
                                                    }
                                        , EI.button (E.alignRight :: Common.buttonStyle)
                                            { onPress = Just FocusCancel
                                            , label = E.text "cancel"
                                            }
                                        ]
                                    ]

                            else
                                E.row [ EE.onClick <| OnRowItemClick date PaymentAmount ]
                                    [ p
                                    ]
                   }
                :: []
        }
    , if model.shownewalloc then
        E.column [ E.width E.fill, EBk.color TC.darkGray, EBd.width 1, E.padding 8, E.spacing TC.defaultSpacing ]
            [ E.row [ EF.bold, E.width E.fill ]
                [ E.text "new allocation"
                , EI.button (E.alignRight :: Common.buttonStyle)
                    { onPress = Just ToggleNewAlloc, label = E.text "-" }
                ]
            , EI.text []
                { onChange = NewAllocDescriptionChanged
                , text = model.allocdescription
                , placeholder = Nothing
                , label = EI.labelLeft [] <| E.text "description"
                }
            , E.row [ E.spacing TC.defaultSpacing ]
                [ EI.text []
                    { onChange = NewAllocHoursChanged
                    , text = model.allochours
                    , placeholder = Nothing
                    , label = EI.labelLeft [] <| E.text "hours"
                    }
                , EI.text [ E.width E.fill ]
                    { onChange = NewAllocCurrencyChanged
                    , text = model.alloccurrency
                    , placeholder = Nothing
                    , label = EI.labelRight [] (model.project.currency |> Maybe.withDefault "" |> E.text)
                    }
                , EI.button Common.buttonStyle { onPress = Just AddAllocationPress, label = E.text "add" }
                ]
            ]

      else
        E.row [ EF.bold, E.width E.fill ]
            [ E.text "new allocation"
            , EI.button (E.alignRight :: Common.buttonStyle)
                { onPress = Just ToggleNewAlloc, label = E.text "+" }
            ]
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


payview : Data.LoginData -> Util.Size -> Time.Zone -> Model -> List (Element Msg)
payview ld size zone model =
    let
        paytote =
            model.payentries
                |> Dict.values
                |> List.foldl (\e t -> t + e.duration) 0

        timetote =
            getTotes model.timeentries
                |> .teammillis

        alloctote =
            model.allocations
                |> Dict.values
                |> List.foldl (\e t -> t + e.duration) 0

        anychecked =
            Dict.foldl (\_ e c -> c || e.checked) False model.payentries
    in
    [ if anychecked then
        E.row [ E.spacing TC.defaultSpacing ]
            [ E.text "checked items: "
            , EI.button Common.buttonStyle
                { onPress = Just <| DeletePayChecked
                , label = E.text "delete"
                }
            ]

      else
        E.none
    , P.view (Dict.size model.payentries) model.pepaginator
    , E.table [ E.spacing TC.defaultSpacing, E.width E.fill ]
        { data =
            Dict.toList model.payentries
                |> P.filter model.pepaginator
        , columns =
            { header =
                EI.checkbox [ E.width E.shrink, E.centerY ]
                    { onChange = CheckPayAll
                    , icon = TC.checkboxIcon
                    , checked =
                        Dict.foldl
                            (\_ pe ac ->
                                ac && pe.checked
                            )
                            True
                            model.payentries
                    , label = EI.labelHidden "check all"
                    }
            , width = E.shrink
            , view =
                \( date, e ) ->
                    EI.checkbox [ E.width E.shrink, E.centerY ]
                        { onChange = CheckPayItem e.paymentdate
                        , icon = TC.checkboxIcon
                        , checked = e.checked
                        , label = EI.labelHidden "check item"
                        }
            }
                :: { header = E.el headerStyle <| E.text "date"
                   , width = E.fill
                   , view =
                        \( date, a ) ->
                            date
                                |> Time.millisToPosix
                                |> Calendar.fromPosix
                                |> (\cdate ->
                                        let
                                            row =
                                                E.row
                                                    [ EE.onClick <| OnRowItemClick date PaymentDate
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
                                        if model.focus == Just ( a.paymentdate, PaymentDate ) then
                                            let
                                                ( display, mbstart ) =
                                                    case Util.parseTime zone model.focuspaydate of
                                                        Err e ->
                                                            ( Util.deadEndsToString e, Nothing )

                                                        Ok Nothing ->
                                                            ( "invalid", Nothing )

                                                        Ok (Just dt) ->
                                                            ( Util.showDateTime zone dt, Just dt )
                                            in
                                            E.column cellEditStyle
                                                [ row
                                                , EI.text [ E.width <| E.px dateTimeWidth ]
                                                    { onChange = FocusPayDateChanged
                                                    , text = model.focuspaydate
                                                    , placeholder = Nothing
                                                    , label = EI.labelHidden "payment date"
                                                    }
                                                , E.text display
                                                , E.row [ E.width E.fill, E.spacing TC.defaultSpacing ]
                                                    [ case mbstart of
                                                        Just start ->
                                                            EI.button Common.buttonStyle
                                                                { onPress = Just <| ChangePaymentDate (Time.posixToMillis start)
                                                                , label = E.text "ok"
                                                                }

                                                        Nothing ->
                                                            EI.button Common.disabledButtonStyle
                                                                { onPress = Nothing
                                                                , label = E.text "ok"
                                                                }
                                                    , EI.button (E.alignRight :: Common.buttonStyle)
                                                        { onPress = Just FocusCancel
                                                        , label = E.text "cancel"
                                                        }
                                                    ]
                                                ]

                                        else
                                            row
                                   )
                   }
                :: { header = E.el headerStyle <| E.text "payee"
                   , width = E.fill
                   , view =
                        \( date, a ) ->
                            if model.focus == Just ( date, PaymentUser ) then
                                E.column cellEditStyle
                                    [ E.row [ EE.onClick <| OnRowItemClick date PaymentUser ]
                                        [ E.text (a.user |> Data.getUserIdVal |> (\i -> Dict.get i model.membernames |> Maybe.withDefault ""))
                                        ]
                                    , EI.button Common.buttonStyle
                                        { onPress = Just SelectPaymentUser
                                        , label = E.text "select member"
                                        }
                                    ]

                            else
                                E.row [ EE.onClick <| OnRowItemClick date PaymentUser ]
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
                            if model.focus == Just ( date, PaymentAmount ) then
                                E.column cellEditStyle
                                    [ E.row [ EE.onClick <| OnRowItemClick date PaymentAmount ]
                                        [ p
                                        ]
                                    , EI.text [ E.width E.fill ]
                                        { onChange = FocusPayChanged
                                        , text = model.focuspay
                                        , placeholder = Nothing
                                        , label = EI.labelHidden "payment"
                                        }
                                    , E.row [ E.width E.fill, E.spacing TC.defaultSpacing ]
                                        [ case String.toFloat model.focuspay of
                                            Just amt ->
                                                EI.button Common.buttonStyle
                                                    { onPress = Just <| ChangePay amt
                                                    , label = E.text "ok"
                                                    }

                                            Nothing ->
                                                EI.button Common.disabledButtonStyle
                                                    { onPress = Nothing
                                                    , label = E.text "ok"
                                                    }
                                        , EI.button (E.alignRight :: Common.buttonStyle)
                                            { onPress = Just FocusCancel
                                            , label = E.text "cancel"
                                            }
                                        ]
                                    ]

                            else
                                E.row [ EE.onClick <| OnRowItemClick date PaymentAmount ]
                                    [ p
                                    ]
                   }
                :: []
        }
    , if model.shownewpayment then
        E.column [ E.width E.fill, EBk.color TC.darkGray, EBd.width 1, E.padding 8, E.spacing TC.defaultSpacing ]
            [ E.row [ EF.bold, E.width E.fill ]
                [ E.text "new payment"
                , EI.button (E.alignRight :: Common.buttonStyle)
                    { onPress = Just ToggleNewPayment, label = E.text "-" }
                ]
            , E.row [ E.spacing TC.defaultSpacing ]
                [ model.paymentuser
                    |> Maybe.andThen
                        (\uid ->
                            Dict.get (Data.getUserIdVal uid) model.membernames
                        )
                    |> Maybe.map (\name -> E.text name)
                    |> Maybe.withDefault (E.el [ EF.italic ] <| E.text "no member selected")
                , EI.button Common.buttonStyle { onPress = Just SelectPaymentUser, label = E.text "..." }
                , EI.text []
                    { onChange = NewPaymentHoursChanged
                    , text = model.paymenthours
                    , placeholder = Nothing
                    , label = EI.labelLeft [] <| E.text "hours"
                    }
                , EI.text []
                    { onChange = NewPaymentCurrencyChanged
                    , text = model.paymentcurrency
                    , placeholder = Nothing
                    , label = EI.labelRight [] (model.project.currency |> Maybe.withDefault "" |> E.text)
                    }
                , case
                    ( model.paymentuser
                    , model.paymenthours
                        |> String.toFloat
                        |> Maybe.map (\x -> x * 60 * 60 * 1000 |> round)
                    )
                  of
                    ( Just uid, Just millis ) ->
                        EI.button Common.buttonStyle { onPress = Just (AddPaymentPress uid millis), label = E.text "add" }

                    _ ->
                        EI.button Common.disabledButtonStyle { onPress = Nothing, label = E.text "add" }
                ]
            ]

      else
        E.row [ EF.bold, E.width E.fill ]
            [ E.text "new payment"
            , EI.button (E.alignRight :: Common.buttonStyle)
                { onPress = Just ToggleNewPayment, label = E.text "+" }
            ]
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


update : Msg -> Model -> Data.LoginData -> Time.Zone -> ( Model, Command )
update msg model ld zone =
    case msg of
        DescriptionChanged t ->
            ( { model | description = t }, None )

        SavePress ->
            ( model, Save (toSaveProjectTime model) )

        RevertPress ->
            ( { model
                | timeentries = setTes model.timeentries model.initialtimeentries
                , payentries = model.initialpayentries
                , allocations = model.initialallocations
              }
            , None
            )

        EditPress ->
            ( model, Edit )

        ImportPress ->
            ( model, GetCsv )

        CsvString str ->
            let
                parseItems : String -> (Csv.Csv -> Result (List String) (List item)) -> Result String (List item)
                parseItems csvstr fn =
                    Csv.parse csvstr
                        |> Result.mapError
                            (List.map
                                Util.deadEndToString
                            )
                        |> Result.andThen
                            (\csv -> fn csv)
                        |> Result.mapError
                            (\strs ->
                                List.intersperse "\n" strs
                                    |> String.concat
                            )
            in
            case model.viewmode of
                Clonks ->
                    case parseItems str (csvToEditTimeEntries zone ld.userid) of
                        Ok el ->
                            ( { model
                                | timeentries =
                                    el
                                        |> List.map (\e -> ( e.startdate, e ))
                                        |> List.foldl (\( k, v ) d -> Dict.insert k v d) (getTes model.timeentries)
                                        |> setTes model.timeentries
                              }
                            , None
                            )

                        Err e ->
                            ( model, ShowError e )

                Team ->
                    ( model, ShowError "csv import is unimplemented for team." )

                Payments ->
                    ( model, ShowError "csv import is unimplemented for payments." )

                Allocations ->
                    case parseItems str (csvToEditAllocations zone ld.userid) of
                        Ok el ->
                            ( { model
                                | allocations =
                                    el
                                        |> List.map (\e -> ( e.allocationdate, e ))
                                        |> List.foldl (\( k, v ) d -> Dict.insert k v d) model.allocations
                              }
                            , None
                            )

                        Err e ->
                            ( model, ShowError e )

                Distributions ->
                    ( model, ShowError "csv import is unimplemented for distributions view." )

        SettingsPress ->
            ( model, Settings )

        SetViewMode vm ->
            ( { model | viewmode = vm }, None )

        ClonkInPress ->
            ( model, GetTime ClonkInTime )

        ClonkOutPress ->
            ( model, GetTime ClonkOutTime )

        ClonkInTime time ->
            let
                nm =
                    { model
                        | timeentries =
                            setTes model.timeentries <|
                                Dict.insert time
                                    { id = Nothing
                                    , description = model.description
                                    , user = ld.userid
                                    , startdate = time
                                    , enddate = time
                                    , ignore = False
                                    , checked = False
                                    }
                                    (getTes model.timeentries)
                    }
            in
            ( nm
            , if model.saveonclonk then
                Save (toSaveProjectTime nm)

              else
                None
            )

        ClonkOutTime time ->
            let
                nm =
                    { model
                        | timeentries =
                            getTes model.timeentries
                                |> Dict.values
                                |> List.filter (.user >> (==) ld.userid)
                                |> List.reverse
                                >> List.head
                                |> Maybe.map (\t -> Dict.insert t.startdate { t | enddate = time } <| getTes model.timeentries)
                                |> Maybe.withDefault (getTes model.timeentries)
                                |> setTes model.timeentries
                    }
            in
            ( nm
            , if model.saveonclonk then
                Save (toSaveProjectTime nm)

              else
                None
            )

        EteDescriptionChanged startdate text ->
            ( { model
                | timeentries =
                    mapTimeentry model.timeentries startdate (\te -> { te | description = text })
              }
            , None
            )

        NowEnd startdate ->
            ( model, GetTime (NowEndTime startdate) )

        NowEndTime startdate enddate ->
            ( { model
                | timeentries =
                    mapTimeentry model.timeentries startdate (\te -> { te | enddate = enddate })
                , focus = Nothing
              }
            , None
            )

        ClearEnd startdate ->
            ( { model
                | timeentries =
                    mapTimeentry model.timeentries startdate (\te -> { te | enddate = startdate })
                , focus = Nothing
              }
            , None
            )

        DeleteClonk startdate ->
            ( { model
                | timeentries =
                    setTes model.timeentries <|
                        Dict.remove startdate (getTes model.timeentries)
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

        SelectPaymentUser ->
            ( model
            , SelectMember (List.map Data.projectMemberToUser model.members)
            )

        AllocDescriptionChanged date text ->
            ( { model
                | allocations =
                    case Dict.get date model.allocations of
                        Just te ->
                            Dict.insert date { te | description = text } model.allocations

                        Nothing ->
                            model.allocations
              }
            , None
            )

        OnRowItemClick i fc ->
            if model.focus == Just ( i, fc ) then
                ( { model | focus = Nothing }, None )

            else
                case model.viewmode of
                    Clonks ->
                        case Dict.get i <| getTes model.timeentries of
                            Just te ->
                                ( { model
                                    | focus = Just ( i, fc )
                                    , focusdescription = te.description
                                    , focusstart = Util.showDateTime zone (Time.millisToPosix te.startdate)
                                    , focusend = Util.showDateTime zone (Time.millisToPosix te.enddate)
                                    , focusduration = millisAsHours (te.enddate - te.startdate)
                                  }
                                , None
                                )

                            Nothing ->
                                ( model, None )

                    Team ->
                        ( model, None )

                    Payments ->
                        case Dict.get i model.payentries of
                            Just pe ->
                                ( { model
                                    | focus = Just ( i, fc )
                                    , focusdescription = ""
                                    , focusstart = ""
                                    , focusend = ""
                                    , focusduration = ""
                                    , focuspay = millisAsHours pe.duration
                                    , focuspaydate = Util.showDateTime zone (Time.millisToPosix pe.paymentdate)
                                  }
                                , None
                                )

                            Nothing ->
                                ( model, None )

                    Allocations ->
                        case Dict.get i model.allocations of
                            Just pe ->
                                ( { model
                                    | focus = Just ( i, fc )
                                    , focusdescription = pe.description
                                    , focusstart = ""
                                    , focusend = ""
                                    , focusduration = millisAsHours pe.duration
                                    , focuspay = millisAsHours pe.duration
                                    , focuspaydate = Util.showDateTime zone (Time.millisToPosix pe.allocationdate)
                                  }
                                , None
                                )

                            Nothing ->
                                ( model, None )

                    Distributions ->
                        case Dict.get i model.payentries of
                            Just pe ->
                                ( { model
                                    | focus = Just ( i, fc )
                                    , focusdescription = ""
                                    , focusstart = ""
                                    , focusend = ""
                                    , focusduration = ""
                                    , focuspay = millisAsHours pe.duration
                                    , focuspaydate = Util.showDateTime zone (Time.millisToPosix pe.paymentdate)
                                  }
                                , None
                                )

                            Nothing ->
                                ( model, None )

        FocusDescriptionChanged text ->
            ( { model | focusdescription = text }, None )

        ChangeDescription ->
            case model.focus of
                Just ( startdate, _ ) ->
                    case model.viewmode of
                        Clonks ->
                            ( { model
                                | timeentries =
                                    mapTimeentry model.timeentries startdate (\te -> { te | description = model.focusdescription })
                                , focus = Nothing
                              }
                            , None
                            )

                        Payments ->
                            ( { model
                                | payentries =
                                    case Dict.get startdate model.payentries of
                                        Just pe ->
                                            Dict.insert startdate { pe | description = model.focusdescription } model.payentries

                                        Nothing ->
                                            model.payentries
                                , focus = Nothing
                              }
                            , None
                            )

                        Allocations ->
                            ( { model
                                | allocations =
                                    case Dict.get startdate model.allocations of
                                        Just pe ->
                                            Dict.insert startdate { pe | description = model.focusdescription } model.allocations

                                        Nothing ->
                                            model.allocations
                                , focus = Nothing
                              }
                            , None
                            )

                        _ ->
                            ( model, None )

                Nothing ->
                    ( model, None )

        FocusStartChanged text ->
            ( { model | focusstart = text }, None )

        ChangeStart newtime ->
            case model.focus of
                Just ( startdate, _ ) ->
                    case Dict.get startdate <| getTes model.timeentries of
                        Just te ->
                            ( { model
                                | timeentries =
                                    setTes model.timeentries <|
                                        (getTes model.timeentries
                                            |> Dict.insert newtime { te | startdate = newtime }
                                            |> Dict.remove startdate
                                        )
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

        ChangeAllocationDate newtime ->
            case model.focus of
                Just ( allocationdate, _ ) ->
                    case Dict.get allocationdate model.allocations of
                        Just pe ->
                            ( { model
                                | allocations =
                                    Dict.insert newtime { pe | allocationdate = newtime } model.allocations
                                        |> Dict.remove allocationdate
                                , focus = Nothing
                                , focuspaydate = ""
                              }
                            , None
                            )

                        Nothing ->
                            ( model, None )

                Nothing ->
                    ( model, None )

        ChangePaymentDate newtime ->
            case model.focus of
                Just ( date, _ ) ->
                    case Dict.get date model.payentries of
                        Just pe ->
                            ( { model
                                | payentries =
                                    Dict.insert newtime { pe | paymentdate = newtime } model.payentries
                                        |> Dict.remove date
                                , focus = Nothing
                                , focuspaydate = ""
                              }
                            , None
                            )

                        Nothing ->
                            ( model, None )

                Nothing ->
                    ( model, None )

        ToggleNewAlloc ->
            ( { model | shownewalloc = not model.shownewalloc }, None )

        ToggleNewPayment ->
            ( { model | shownewpayment = not model.shownewpayment }, None )

        FocusEndChanged text ->
            ( { model | focusend = text }, None )

        FocusCancel ->
            ( { model | focus = Nothing }, None )

        ChangeEnd startdate enddate ->
            ( { model
                | timeentries = mapTimeentry model.timeentries startdate (\te -> { te | enddate = enddate })
                , focusend = ""
                , focus = Nothing
              }
            , None
            )

        FocusDurationChanged text ->
            ( { model | focusduration = text }, None )

        FocusPayChanged s ->
            ( { model
                | focuspay = s
              }
            , None
            )

        ChangePay f ->
            ( { model
                | focus = Nothing
                , payentries =
                    model.focus
                        |> Maybe.map Tuple.first
                        |> Maybe.andThen (\r -> Dict.get r model.payentries)
                        |> Maybe.map (\pe -> Dict.insert pe.paymentdate { pe | duration = round <| f * 60 * 60 * 1000 } model.payentries)
                        |> Maybe.withDefault model.payentries
              }
            , None
            )

        FocusAllocationChanged s ->
            ( { model
                | focuspay = s
              }
            , None
            )

        ChangeAllocation amt ->
            ( { model
                | allocations =
                    model.focus
                        |> Maybe.map Tuple.first
                        |> Maybe.andThen (\r -> Dict.get r model.allocations)
                        |> Maybe.map (\e -> Dict.insert e.allocationdate { e | duration = round <| amt * 60 * 60 * 1000 } model.allocations)
                        |> Maybe.withDefault model.allocations
                , focus = Nothing
              }
            , None
            )

        NewAllocDescriptionChanged s ->
            ( { model | allocdescription = s }, None )

        NewAllocHoursChanged s ->
            ( { model
                | allochours = s
                , alloccurrency =
                    s
                        |> String.toFloat
                        |> Maybe.andThen
                            (\h ->
                                model.project.rate |> Maybe.map (\r -> r * h |> String.fromFloat)
                            )
                        |> Maybe.withDefault ""
              }
            , None
            )

        NewAllocCurrencyChanged s ->
            ( { model
                | alloccurrency = s
                , allochours =
                    s
                        |> String.toFloat
                        |> Maybe.andThen
                            (\c ->
                                model.project.rate |> Maybe.map (\r -> c / r |> String.fromFloat)
                            )
                        |> Maybe.withDefault ""
              }
            , None
            )

        NewPaymentHoursChanged s ->
            ( { model
                | paymenthours = s
                , paymentcurrency =
                    s
                        |> String.toFloat
                        |> Maybe.andThen
                            (\h ->
                                model.project.rate |> Maybe.map (\r -> r * h |> String.fromFloat)
                            )
                        |> Maybe.withDefault ""
              }
            , None
            )

        NewPaymentCurrencyChanged s ->
            ( { model
                | paymentcurrency = s
                , paymenthours =
                    s
                        |> String.toFloat
                        |> Maybe.andThen
                            (\c ->
                                model.project.rate |> Maybe.map (\r -> c / r |> String.fromFloat)
                            )
                        |> Maybe.withDefault ""
              }
            , None
            )

        AddAllocationPress ->
            ( model, GetTime AddAllocation )

        AddAllocation now ->
            case String.toFloat model.allochours of
                Just hours ->
                    ( { model
                        | allocations =
                            Dict.insert now
                                { id = Nothing
                                , description = model.allocdescription
                                , allocationdate = now
                                , duration = hours * 60 * 60 * 1000 |> round
                                , checked = False
                                }
                                model.allocations
                      }
                    , None
                    )

                Nothing ->
                    ( model, None )

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
                            TR.teamMillisPerDay zone (Dict.values <| getTes model.timeentries)

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
                    setTes model.timeentries <|
                        Dict.map
                            (\_ te ->
                                if te.user == ld.userid then
                                    { te | checked = c }

                                else
                                    te
                            )
                            (getTes model.timeentries)
              }
            , None
            )

        CheckItem sd c ->
            ( { model
                | timeentries = mapTimeentry model.timeentries sd (\te -> { te | checked = c })
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

        CheckAllocAll c ->
            ( { model
                | allocations =
                    Dict.map
                        (\_ a ->
                            { a | checked = c }
                        )
                        model.allocations
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

        CheckAllocationItem sd c ->
            ( { model
                | allocations =
                    model.allocations
                        |> Dict.get sd
                        |> Maybe.map (\pe -> Dict.insert sd { pe | checked = c } model.allocations)
                        |> Maybe.withDefault model.allocations
              }
            , None
            )

        DeleteChecked ->
            ( { model
                | timeentries =
                    setTes model.timeentries <|
                        Dict.filter (\_ te -> not te.checked) (getTes model.timeentries)
              }
            , None
            )

        DeletePayChecked ->
            ( { model
                | payentries = Dict.filter (\_ pe -> not pe.checked) model.payentries
              }
            , None
            )

        DeleteAllocationChecked ->
            ( { model
                | allocations = Dict.filter (\_ pe -> not pe.checked) model.allocations
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
                        (getTes model.timeentries)

                igftn =
                    if anyunignored then
                        always True

                    else
                        not
            in
            ( { model
                | timeentries =
                    setTes model.timeentries <|
                        Dict.map
                            (\_ te ->
                                if te.checked then
                                    { te | ignore = igftn te.ignore }

                                else
                                    te
                            )
                            (getTes model.timeentries)
              }
            , None
            )

        ExportAll ->
            ( model
            , SaveCsv ("timeclonk-" ++ model.project.name ++ ".csv") (eteToCsv zone model.project.name model.membernames (getTes model.timeentries |> Dict.values))
            )

        TeForward ->
            ( { model | tepaginator = P.onForward model.tepaginator }, None )

        TeBack c ->
            ( { model | tepaginator = P.onBack c model.tepaginator }, None )

        TeToStart ->
            ( { model | tepaginator = P.onToStart model.tepaginator }, None )

        TeToEnd c ->
            ( { model | tepaginator = P.onToEnd c model.tepaginator }, None )

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
