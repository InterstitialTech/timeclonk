module ProjectEdit exposing (..)

import Common
import Data
import Element as E exposing (Element)
import Element.Background as EBk
import Element.Border as EBd
import Element.Font as EF
import Element.Input as EI
import Orgauth.Data exposing (UserId, getUserIdVal, makeUserId)
import Route
import TDict exposing (TDict)
import TangoColors as TC
import TcCommon as TC
import Toop
import Util
import WindowKeys as WK


type Msg
    = NameChanged String
    | DescriptionChanged String
    | DueDaysChanged String
    | InvoiceIdTemplateChanged String
    | InvoiceSeqChanged String
    | PayerChanged String
    | PayeeChanged String
    | RateChanged String
    | GenericTaskChanged String
    | CurrencyChanged String
    | SavePress
    | RevertPress
    | DonePress
    | SettingsPress
    | NewPress
    | AddMemberPress
    | SelectRolePress UserId
    | TogglePublic Bool
    | Noop


type alias Model =
    { id : Maybe Data.ProjectId
    , name : String
    , description : String
    , dueDays : Maybe Int
    , extraFields : List Data.ExtraField
    , invoiceIdTemplate : String
    , invoiceSeq : Int
    , payer : String
    , payee : String
    , genericTask : String
    , public : Bool
    , ratestring : String
    , currency : String
    , createdate : Maybe Int
    , changeddate : Maybe Int
    , members : TDict UserId Int Data.ProjectMember
    , initialProject : Maybe Data.Project
    , initialMembers : TDict UserId Int Data.ProjectMember
    }


type Command
    = Save Data.SaveProjectEdit
    | New
    | AddMember
    | SelectRole UserId
    | Done
    | Settings
    | None


onWkKeyPress : WK.Key -> Model -> Data.LoginData -> ( Model, Command )
onWkKeyPress key model ld =
    case Toop.T4 key.key key.ctrl key.alt key.shift of
        Toop.T4 "s" True False False ->
            if isDirty model then
                update SavePress model ld

            else
                ( model, None )

        _ ->
            ( model, None )


toSaveProject : Model -> Data.SaveProject
toSaveProject model =
    let
        ( r, c ) =
            ( model.ratestring |> String.toFloat
            , case model.currency of
                "" ->
                    Nothing

                x ->
                    Just x
            )

        ( rate, currency ) =
            case ( r, c ) of
                ( Just rt, Just cy ) ->
                    ( Just rt, Just cy )

                _ ->
                    ( Nothing, Nothing )
    in
    { id = model.id
    , name = model.name
    , description = model.description
    , dueDays = model.dueDays
    , extraFields = model.extraFields
    , invoiceIdTemplate = model.invoiceIdTemplate
    , invoiceSeq = model.invoiceSeq
    , payer = model.payer
    , payee = model.payee
    , genericTask = model.genericTask
    , public = model.public
    , rate = rate
    , currency = currency
    }


toSaveProjectEdit : Model -> Data.SaveProjectEdit
toSaveProjectEdit model =
    { project = toSaveProject model
    , members =
        (TDict.diff model.members model.initialMembers
            |> TDict.values
            |> List.map (\m -> { id = m.id, delete = False, role = m.role })
        )
            ++ (TDict.diff model.initialMembers model.members
                    |> TDict.values
                    |> List.map (\m -> { id = m.id, delete = True, role = m.role })
               )
            ++ (model.initialMembers
                    |> TDict.values
                    |> List.filterMap
                        (\member ->
                            TDict.get member.id model.members
                                |> Maybe.andThen
                                    (\m ->
                                        if m.role == member.role then
                                            Nothing

                                        else
                                            Just m
                                    )
                        )
                    |> List.map (\m -> { id = m.id, delete = False, role = m.role })
               )
    }


emptyUmDict : TDict UserId Int Data.ProjectMember
emptyUmDict =
    TDict.empty getUserIdVal makeUserId


onSavedProjectInvoice : Data.Project -> Model -> Model
onSavedProjectInvoice project model =
    let
        members =
            TDict.values model.members
    in
    initEdit project members


onSavedProjectEdit : Data.SavedProjectEdit -> Model -> Model
onSavedProjectEdit spe model =
    let
        mbrs =
            spe.members |> List.map (\m -> ( m.id, m )) |> TDict.insertList emptyUmDict
    in
    { model
        | id = Just spe.project.id
        , changeddate = Just spe.project.changeddate
        , createdate =
            model.createdate
                |> Maybe.withDefault spe.project.changeddate
                |> Just
        , members = mbrs
        , initialMembers = mbrs
        , initialProject = Just spe.project
    }


addMember : Data.User -> Data.Role -> Model -> Model
addMember pm role model =
    { model | members = TDict.insert pm.id { id = pm.id, name = pm.name, role = role } model.members }


setRole : ( UserId, Data.Role ) -> Model -> Model
setRole ( id, role ) model =
    { model
        | members =
            TDict.update id
                (Maybe.map
                    (\user -> { user | role = role })
                )
                model.members
    }


isDirty : Model -> Bool
isDirty model =
    let
        projdirty =
            model.initialProject
                |> Maybe.map
                    (\ip ->
                        not
                            ((model.id == Just ip.id)
                                && (model.name == ip.name)
                                && (model.description == ip.description)
                                && (model.dueDays == ip.dueDays)
                                && (model.extraFields == ip.extraFields)
                                && (model.invoiceIdTemplate == ip.invoiceIdTemplate)
                                && (model.invoiceSeq == ip.invoiceSeq)
                                && (model.payer == ip.payer)
                                && (model.payee == ip.payee)
                                && (model.public == ip.public)
                                && (model.ratestring
                                        == (ip.rate |> Maybe.map String.fromFloat |> Maybe.withDefault "")
                                   )
                                && (model.currency
                                        == (ip.currency |> Maybe.withDefault "")
                                   )
                                && (model.createdate == Just ip.createdate)
                                && (model.changeddate == Just ip.changeddate)
                            )
                    )
                |> Maybe.withDefault True

        membersdirty =
            model.members /= model.initialMembers
    in
    projdirty || membersdirty


initNew : Data.LoginData -> Model
initNew ld =
    { id = Nothing
    , name = ""
    , description = ""
    , dueDays = Nothing
    , extraFields = []
    , invoiceIdTemplate = "example<seq>"
    , invoiceSeq = 0
    , payer = ""
    , payee = ""
    , genericTask = ""
    , public = False
    , ratestring = ""
    , currency = ""
    , createdate = Nothing
    , changeddate = Nothing
    , members = TDict.insert ld.userid { id = ld.userid, name = ld.name, role = Data.Admin } emptyUmDict
    , initialProject = Nothing
    , initialMembers = emptyUmDict
    }


initEdit : Data.Project -> List Data.ProjectMember -> Model
initEdit proj members =
    let
        mbs =
            members
                |> List.map (\m -> ( m.id, m ))
                |> TDict.insertList emptyUmDict
    in
    { id = Just proj.id
    , name = proj.name
    , description = proj.description
    , dueDays = proj.dueDays
    , extraFields = proj.extraFields
    , invoiceIdTemplate = proj.invoiceIdTemplate
    , invoiceSeq = proj.invoiceSeq
    , payer = proj.payer
    , payee = proj.payee
    , genericTask = proj.genericTask
    , public = proj.public
    , ratestring = proj.rate |> Maybe.map String.fromFloat |> Maybe.withDefault ""
    , currency = proj.currency |> Maybe.withDefault ""
    , createdate = Just proj.createdate
    , changeddate = Just proj.changeddate
    , members = mbs
    , initialProject = Just proj
    , initialMembers = mbs
    }


view : Data.LoginData -> Util.Size -> Model -> Element Msg
view ld size model =
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
            [ E.row [ E.spacing TC.defaultSpacing, E.width E.fill ]
                [ E.row [ EF.bold ] [ E.text ld.name ]
                , EI.button
                    (E.alignRight :: Common.buttonStyle)
                    { onPress = Just SettingsPress, label = E.text "settings" }
                ]
            , E.row [ E.spacing TC.defaultSpacing ] <|
                [ EI.button Common.buttonStyle { onPress = Just DonePress, label = E.text "<-" }
                , EI.button Common.buttonStyle { onPress = Just NewPress, label = E.text "new" }
                ]
                    ++ (if isdirty then
                            [ EI.button
                                (Common.buttonStyle ++ [ EBk.color TC.darkYellow ])
                                { onPress = Just SavePress, label = E.text "save" }
                            ]

                        else
                            []
                       )
            , E.column
                [ E.padding 8
                , EBd.rounded 10
                , EBd.width 1
                , EBd.color TC.darkGrey
                , EBk.color TC.white
                , E.spacing TC.defaultSpacing
                ]
                [ E.row [ EF.bold, E.centerX, EF.size 20 ]
                    [ E.text "All Projects" ]
                , EI.text
                    (if isdirty then
                        [ E.focused [ EBd.glow TC.darkYellow 3 ] ]

                     else
                        []
                    )
                    { onChange =
                        NameChanged
                    , text = model.name
                    , placeholder = Nothing
                    , label =
                        EI.labelLeft
                            []
                            (E.text "name")
                    }
                , EI.multiline
                    (if isdirty then
                        [ E.focused [ EBd.glow TC.darkYellow 3 ] ]

                     else
                        []
                    )
                    { onChange =
                        DescriptionChanged
                    , text = model.description
                    , placeholder = Nothing
                    , label =
                        EI.labelLeft
                            []
                            (E.text "description")
                    , spellcheck = True
                    }
                , E.row [ E.spacing 8 ]
                    [ EI.checkbox []
                        { onChange = TogglePublic
                        , icon = EI.defaultCheckbox
                        , checked = model.public
                        , label = EI.labelLeft [] (E.text "public")
                        }
                    , case ( model.public, model.id ) of
                        ( True, Just id ) ->
                            let
                                u =
                                    Route.routeUrl <| Route.ProjectViewR (Data.getProjectIdVal id) "team"
                            in
                            E.link Common.linkStyle
                                { url = u
                                , label = E.text u
                                }

                        _ ->
                            E.none
                    ]
                , E.row [ EF.bold, E.centerX, EF.size 20 ]
                    [ E.text "For Distributions" ]
                , EI.text
                    (if isdirty then
                        [ E.focused [ EBd.glow TC.darkYellow 3 ] ]

                     else
                        []
                    )
                    { onChange =
                        RateChanged
                    , text = model.ratestring
                    , placeholder = Nothing
                    , label =
                        EI.labelLeft
                            []
                            (E.text "rate")
                    }
                , EI.text
                    (if isdirty then
                        [ E.focused [ EBd.glow TC.darkYellow 3 ] ]

                     else
                        []
                    )
                    { onChange =
                        CurrencyChanged
                    , text = model.currency
                    , placeholder = Nothing
                    , label =
                        EI.labelLeft
                            []
                            (E.text "currency")
                    }
                , E.row [ EF.bold, E.centerX, EF.size 20 ]
                    [ E.text "For Invoices" ]
                , EI.text
                    (if isdirty then
                        [ E.focused [ EBd.glow TC.darkYellow 3 ] ]

                     else
                        []
                    )
                    { onChange =
                        InvoiceIdTemplateChanged
                    , text = model.invoiceIdTemplate
                    , placeholder = Nothing
                    , label =
                        EI.labelLeft
                            []
                            (E.text "invoice id template")
                    }
                , E.row [ E.alignRight ]
                    [ E.text "Use <date> or <seq>, ex: "
                    , E.el [ EF.italic ] <| E.text "example<date><seq>"
                    ]
                , EI.text
                    (if isdirty then
                        [ E.focused [ EBd.glow TC.darkYellow 3 ] ]

                     else
                        []
                    )
                    { onChange =
                        InvoiceSeqChanged
                    , text = String.fromInt model.invoiceSeq
                    , placeholder = Nothing
                    , label =
                        EI.labelLeft
                            []
                            (E.text "invoice sequence number")
                    }
                , EI.text
                    (if isdirty then
                        [ E.focused [ EBd.glow TC.darkYellow 3 ] ]

                     else
                        []
                    )
                    { onChange =
                        GenericTaskChanged
                    , text = model.genericTask
                    , placeholder = Nothing
                    , label =
                        EI.labelLeft
                            []
                            (E.text "generic task description")
                    }
                , EI.multiline
                    (if isdirty then
                        [ E.focused [ EBd.glow TC.darkYellow 3 ] ]

                     else
                        []
                    )
                    { onChange =
                        PayerChanged
                    , text = model.payer
                    , placeholder = Nothing
                    , label =
                        EI.labelLeft
                            []
                            (E.text "payer")
                    , spellcheck = True
                    }
                , EI.multiline
                    (if isdirty then
                        [ E.focused [ EBd.glow TC.darkYellow 3 ] ]

                     else
                        []
                    )
                    { onChange =
                        PayeeChanged
                    , text = model.payee
                    , placeholder = Nothing
                    , label =
                        EI.labelLeft
                            []
                            (E.text "payee")
                    , spellcheck = True
                    }
                , EI.text
                    (if isdirty then
                        [ E.focused [ EBd.glow TC.darkYellow 3 ] ]

                     else
                        []
                    )
                    { onChange =
                        DueDaysChanged
                    , text = model.dueDays |> Maybe.map (\d -> String.fromInt d) |> Maybe.withDefault ""
                    , placeholder = Nothing
                    , label =
                        EI.labelLeft
                            []
                            (E.text "due date (days)")
                    }
                ]
            , E.column
                [ E.padding 8
                , EBd.rounded 10
                , EBd.width 1
                , EBd.color TC.darkGrey
                , EBk.color TC.white
                , E.spacing TC.defaultSpacing
                ]
                -- table listing member roles.
                [ E.row [ E.spacing 10 ]
                    [ E.el [ EF.bold ] <| E.text "members"
                    , EI.button Common.buttonStyle { onPress = Just AddMemberPress, label = E.text "add" }
                    ]
                , E.table [ E.spacing 10 ]
                    { data = TDict.values model.members
                    , columns =
                        [ { header = E.el [ EF.underline ] <| E.text "member", width = E.shrink, view = \m -> E.text m.name }
                        , { header = E.el [ EF.underline ] <| E.text "role"
                          , width = E.shrink
                          , view =
                                \m ->
                                    EI.button Common.buttonStyle
                                        { onPress = Just <| SelectRolePress m.id
                                        , label = E.text (Data.showRole m.role)
                                        }
                          }
                        ]
                    }
                ]
            ]


update : Msg -> Model -> Data.LoginData -> ( Model, Command )
update msg model ld =
    case msg of
        NameChanged t ->
            ( { model | name = t }, None )

        DescriptionChanged t ->
            ( { model | description = t }, None )

        DueDaysChanged days ->
            ( { model | dueDays = String.toInt days }, None )

        InvoiceIdTemplateChanged t ->
            ( { model | invoiceIdTemplate = t }, None )

        InvoiceSeqChanged t ->
            case String.toInt t of
                Just i ->
                    ( { model | invoiceSeq = i }, None )

                Nothing ->
                    ( model, None )

        PayerChanged t ->
            ( { model | payer = t }, None )

        PayeeChanged t ->
            ( { model | payee = t }, None )

        GenericTaskChanged t ->
            ( { model | genericTask = t }, None )

        RateChanged t ->
            ( { model | ratestring = t }, None )

        CurrencyChanged t ->
            ( { model | currency = t }, None )

        SavePress ->
            ( model, Save (toSaveProjectEdit model) )

        RevertPress ->
            ( model, None )

        NewPress ->
            ( model, New )

        DonePress ->
            ( model, Done )

        SettingsPress ->
            ( model, Settings )

        AddMemberPress ->
            ( model, AddMember )

        SelectRolePress id ->
            ( model, SelectRole id )

        TogglePublic b ->
            ( { model | public = b }, None )

        Noop ->
            ( model, None )
