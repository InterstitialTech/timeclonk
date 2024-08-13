module Data exposing
    ( Allocation
    , AllocationId(..)
    , InvoiceItem
    , ListProject
    , LoginData
    , PayEntry
    , PayEntryId(..)
    , PayType(..)
    , PrintInvoice
    , PrintInvoiceInternal
    , Project
    , ProjectEdit
    , ProjectId(..)
    , ProjectMember
    , ProjectTime
    , Role(..)
    , SaveAllocation
    , SavePayEntry
    , SaveProject
    , SaveProjectEdit
    , SaveProjectInvoice
    , SaveProjectMember
    , SaveProjectTime
    , SaveTimeEntry
    , SavedProject
    , SavedProjectEdit
    , TimeEntry
    , TimeEntryId(..)
    , User
    , UserInviteData
    , UserInviteProject
    , decodeAllocation
    , decodeListProject
    , decodePayEntry
    , decodeProject
    , decodeProjectEdit
    , decodeProjectMember
    , decodeProjectTime
    , decodeRole
    , decodeSavedProject
    , decodeSavedProjectEdit
    , decodeTimeEntry
    , decodeUser
    , encodePrintInvoice
    , encodeRole
    , encodeSaveAllocation
    , encodeSavePayEntry
    , encodeSaveProject
    , encodeSaveProjectEdit
    , encodeSaveProjectInvoice
    , encodeSaveProjectMember
    , encodeSaveProjectTime
    , encodeSaveTimeEntry
    , encodeUserInviteData
    , encodeUserInviteProject
    , getAllocationIdVal
    , getPayEntryIdVal
    , getProjectIdVal
    , getTimeEntryIdVal
    , ldToOdLd
    , makeAllocationId
    , makeInvoiceId
    , makePayEntryId
    , makeProjectId
    , makeTimeEntryId
    , odLdToLd
    , piDate
    , projectMemberToUser
    , roleToString
    , showRole
    , stringToRole
    , toPi
      -- , toPrintInvoice
    , toSaveProjectInvoice
    )

import Json.Decode as JD
import Json.Encode as JE
import Orgauth.Data as OD exposing (UserId, getUserIdVal, makeUserId)
import Time
import UUID exposing (UUID)
import Url.Builder as UB
import Util exposing (andMap)


type alias LoginData =
    { userid : UserId
    , name : String
    , email : String
    , admin : Bool
    , active : Bool
    }


ldToOdLd : LoginData -> OD.LoginData
ldToOdLd ld =
    { userid = ld.userid
    , name = ld.name
    , email = ld.email
    , admin = ld.admin
    , active = ld.active
    , data = JE.null
    }


odLdToLd : OD.LoginData -> LoginData
odLdToLd ld =
    { userid = ld.userid
    , name = ld.name
    , email = ld.email
    , admin = ld.admin
    , active = ld.active
    }



----------------------------------------
-- Timeclonk specific types.
----------------------------------------


type alias UserInviteProject =
    { id : ProjectId, role : Role }


encodeUserInviteProject : UserInviteProject -> JE.Value
encodeUserInviteProject p =
    JE.object
        [ ( "id", JE.int <| getProjectIdVal p.id )
        , ( "role", encodeRole p.role )
        ]


type alias UserInviteData =
    { projects : List UserInviteProject }


encodeUserInviteData : UserInviteData -> JE.Value
encodeUserInviteData d =
    JE.object
        [ ( "projects", JE.list encodeUserInviteProject d.projects )
        ]


type alias ListProject =
    { id : ProjectId
    , name : String
    , role : Role
    }


type alias Project =
    { id : ProjectId
    , name : String
    , description : String
    , dueDays : Maybe Int
    , extraFields : List ( String, String )
    , invoiceIdTemplate : String
    , invoiceSeq : Int
    , payer : String
    , payee : String
    , genericTask : String
    , public : Bool
    , rate : Maybe Float
    , currency : Maybe String
    , createdate : Int
    , changeddate : Int
    }


type alias SaveProjectInvoice =
    { id : ProjectId
    , extraFields : List ( String, String )
    , invoiceSeq : Int
    }


type alias SaveProject =
    { id : Maybe ProjectId
    , name : String
    , description : String
    , dueDays : Maybe Int
    , extraFields : List ( String, String )
    , invoiceIdTemplate : String
    , invoiceSeq : Int
    , payer : String
    , payee : String
    , genericTask : String
    , public : Bool
    , rate : Maybe Float
    , currency : Maybe String
    }


type alias SavedProject =
    { id : ProjectId
    , changeddate : Int
    }


type Role
    = Member
    | Admin
    | Observer


type alias ProjectMember =
    { id : UserId
    , name : String
    , role : Role
    }


showRole : Role -> String
showRole r =
    case r of
        Member ->
            "member"

        Admin ->
            "admin"

        Observer ->
            "observer"


projectMemberToUser : ProjectMember -> User
projectMemberToUser pm =
    { id = pm.id
    , name = pm.name
    }


type alias User =
    { id : UserId
    , name : String
    }


type alias ProjectEdit =
    { project : Project
    , members : List ProjectMember
    }


type alias SaveProjectMember =
    { id : UserId
    , role : Role
    , delete : Bool
    }


type alias SaveProjectEdit =
    { project : SaveProject
    , members : List SaveProjectMember
    }


type alias SavedProjectEdit =
    { project : Project
    , members : List ProjectMember
    }


type alias TimeEntry =
    { id : TimeEntryId
    , project : ProjectId
    , user : UserId
    , description : String
    , startdate : Int
    , enddate : Int
    , ignore : Bool
    , createdate : Int
    , changeddate : Int
    , creator : UserId
    }


type alias SaveTimeEntry =
    { id : Maybe TimeEntryId
    , project : ProjectId
    , user : UserId
    , description : String
    , startdate : Int
    , enddate : Int
    , ignore : Bool
    }


type PayType
    = Invoiced
    | Paid


type alias PayEntry =
    { id : PayEntryId
    , project : ProjectId
    , user : UserId
    , duration : Int
    , paytype : PayType
    , paymentdate : Int
    , description : String
    , createdate : Int
    , changeddate : Int
    , creator : UserId
    }


type alias SavePayEntry =
    { id : Maybe PayEntryId
    , project : ProjectId
    , user : UserId
    , duration : Int
    , paytype : PayType
    , paymentdate : Int
    , description : String
    }


type alias Allocation =
    { id : AllocationId
    , project : ProjectId
    , duration : Int
    , allocationdate : Int
    , description : String
    , createdate : Int
    , changeddate : Int
    , creator : UserId
    }


type alias SaveAllocation =
    { id : Maybe AllocationId
    , project : ProjectId
    , duration : Int
    , allocationdate : Int
    , description : String
    }


type alias SaveProjectTime =
    { project : ProjectId
    , savetimeentries : List SaveTimeEntry
    , deletetimeentries : List TimeEntryId
    , savepayentries : List SavePayEntry
    , deletepayentries : List PayEntryId
    , saveallocations : List SaveAllocation
    , deleteallocations : List AllocationId
    }


type alias ProjectTime =
    { project : Project
    , members : List ProjectMember
    , timeentries : List TimeEntry
    , payentries : List PayEntry
    , allocations : List Allocation
    }


type alias InvoiceItem =
    { description : String
    , duration : Float
    , rate : Float
    }


type alias PrintInvoiceInternal =
    { projectid : ProjectId
    , seq : Int
    , idtemplate : String
    , payer : String
    , payee : String
    , items : List InvoiceItem
    , extraFields : List ( String, String )
    }


makeInvoiceId : String -> String -> Int -> String
makeInvoiceId template date seq =
    template
        |> String.replace "<seq>" (String.fromInt seq)
        |> String.replace "<date>" date


piDate : Time.Posix -> Time.Zone -> String
piDate time zone =
    (String.fromInt <| Time.toYear zone time)
        ++ "-"
        ++ (String.fromInt <| Util.monthInt <| Time.toMonth zone time)
        ++ "-"
        ++ (String.fromInt <| Time.toDay zone time)


toPi :
    PrintInvoiceInternal
    -> String
    -> String
    -> PrintInvoice
toPi pii date duedate =
    { id = makeInvoiceId pii.idtemplate date pii.seq
    , payer = pii.payer
    , payee = pii.payee
    , items = pii.items
    , date = date
    , dueDate = duedate
    , extraFields = pii.extraFields
    }


type alias PrintInvoice =
    { id : String
    , payer : String
    , payee : String
    , items : List InvoiceItem
    , date : String
    , dueDate : String
    , extraFields : List ( String, String )
    }


toSaveProjectInvoice : PrintInvoiceInternal -> SaveProjectInvoice
toSaveProjectInvoice pi =
    { id = pi.projectid
    , extraFields = pi.extraFields
    , invoiceSeq = pi.seq
    }


encodeInvoiceItem : InvoiceItem -> JE.Value
encodeInvoiceItem ii =
    JE.object
        [ ( "description", JE.string ii.description )
        , ( "duration", JE.float ii.duration )
        , ( "rate", JE.float ii.rate )
        ]


encodePrintInvoice : PrintInvoice -> JE.Value
encodePrintInvoice pi =
    JE.object
        [ ( "id", JE.string pi.id )
        , ( "payer", JE.string pi.payer )
        , ( "payee", JE.string pi.payee )
        , ( "date", JE.string pi.date )
        , ( "due_date", JE.string pi.dueDate )
        , ( "items", JE.list encodeInvoiceItem pi.items )
        ]



-------------------------------------------
-- Id types.  They're all ints underneath.
-------------------------------------------


type ProjectId
    = ProjectId Int


makeProjectId : Int -> ProjectId
makeProjectId i =
    ProjectId i


getProjectIdVal : ProjectId -> Int
getProjectIdVal uid =
    case uid of
        ProjectId i ->
            i


type PayEntryId
    = PayEntryId Int


makePayEntryId : Int -> PayEntryId
makePayEntryId i =
    PayEntryId i


getPayEntryIdVal : PayEntryId -> Int
getPayEntryIdVal uid =
    case uid of
        PayEntryId i ->
            i


type TimeEntryId
    = TimeEntryId Int


makeTimeEntryId : Int -> TimeEntryId
makeTimeEntryId i =
    TimeEntryId i


getTimeEntryIdVal : TimeEntryId -> Int
getTimeEntryIdVal uid =
    case uid of
        TimeEntryId i ->
            i


type AllocationId
    = AllocationId Int


makeAllocationId : Int -> AllocationId
makeAllocationId i =
    AllocationId i


getAllocationIdVal : AllocationId -> Int
getAllocationIdVal uid =
    case uid of
        AllocationId i ->
            i


decodePayType : JD.Decoder PayType
decodePayType =
    JD.string
        |> JD.andThen
            (\i ->
                case i of
                    "Invoiced" ->
                        JD.succeed Invoiced

                    "Paid" ->
                        JD.succeed Paid

                    x ->
                        JD.fail ("invalid paytype: " ++ x)
            )


encodePayType : PayType -> JE.Value
encodePayType pt =
    case pt of
        Invoiced ->
            JE.string "Invoiced"

        Paid ->
            JE.string "Paid"



----------------------------------------
-- Json encoders/decoders
----------------------------------------


decodeListProject : JD.Decoder ListProject
decodeListProject =
    JD.succeed ListProject
        |> andMap (JD.field "id" JD.int |> JD.map makeProjectId)
        |> andMap (JD.field "name" JD.string)
        |> andMap (JD.field "role" decodeRole)


encodeSaveProjectInvoice : SaveProjectInvoice -> JE.Value
encodeSaveProjectInvoice sp =
    JE.object <|
        [ ( "id", JE.int (getProjectIdVal sp.id) )
        , ( "extra_fields", encodeExtraFields sp.extraFields )
        , ( "invoice_seq", JE.int sp.invoiceSeq )
        ]


encodeSaveProject : SaveProject -> JE.Value
encodeSaveProject sp =
    JE.object <|
        List.filterMap identity
            [ Just ( "name", JE.string sp.name )
            , Just ( "description", JE.string sp.description )
            , sp.dueDays |> Maybe.map (\dd -> ( "due_days", JE.int dd ))
            , Just ( "extra_fields", encodeExtraFields sp.extraFields )
            , Just ( "invoice_id_template", JE.string sp.invoiceIdTemplate )
            , Just ( "invoice_seq", JE.int sp.invoiceSeq )
            , Just ( "payer", JE.string sp.payer )
            , Just ( "payee", JE.string sp.payee )
            , Just ( "generic_task", JE.string sp.genericTask )
            , Just ( "public", JE.bool sp.public )
            , sp.rate |> Maybe.map (\rate -> ( "rate", JE.float rate ))
            , sp.currency |> Maybe.map (\currency -> ( "currency", JE.string currency ))
            , sp.id |> Maybe.map (\id -> ( "id", JE.int (getProjectIdVal id) ))
            ]


encodeExtraField : ( String, String ) -> JE.Value
encodeExtraField ( n, v ) =
    JE.object [ ( "name", JE.string n ), ( "value", JE.string v ) ]


encodeExtraFields : List ( String, String ) -> JE.Value
encodeExtraFields fields =
    JE.object <| (fields |> List.map (\( n, v ) -> ( n, JE.string v )))


decodeExtraFields : JD.Decoder (List ( String, String ))
decodeExtraFields =
    JD.keyValuePairs JD.string


decodeProject : JD.Decoder Project
decodeProject =
    JD.succeed Project
        |> andMap (JD.field "id" JD.int |> JD.map makeProjectId)
        |> andMap (JD.field "name" JD.string)
        |> andMap (JD.field "description" JD.string)
        |> andMap (JD.field "due_days" (JD.maybe JD.int))
        |> andMap (JD.field "extra_fields" decodeExtraFields)
        |> andMap (JD.field "invoice_id_template" JD.string)
        |> andMap (JD.field "invoice_seq" JD.int)
        |> andMap (JD.field "payer" JD.string)
        |> andMap (JD.field "payee" JD.string)
        |> andMap (JD.field "generic_task" JD.string)
        |> andMap (JD.field "public" JD.bool)
        |> andMap (JD.field "rate" <| JD.maybe JD.float)
        |> andMap (JD.field "currency" <| JD.maybe JD.string)
        |> andMap (JD.field "createdate" JD.int)
        |> andMap (JD.field "changeddate" JD.int)


decodeSavedProject : JD.Decoder SavedProject
decodeSavedProject =
    JD.succeed SavedProject
        |> andMap (JD.field "id" JD.int |> JD.map makeProjectId)
        |> andMap (JD.field "changeddate" JD.int)


stringToRole : String -> Result String Role
stringToRole s =
    case s of
        "Member" ->
            Ok Member

        "Admin" ->
            Ok Admin

        "Observer" ->
            Ok Observer

        _ ->
            Err ("invalid role string: " ++ s)


roleToString : Role -> String
roleToString r =
    case r of
        Member ->
            "Member"

        Admin ->
            "Admin"

        Observer ->
            "Observer"


decodeRole : JD.Decoder Role
decodeRole =
    JD.string
        |> JD.andThen
            (\s ->
                case stringToRole s of
                    Ok r ->
                        JD.succeed r

                    Err e ->
                        JD.fail e
            )


encodeRole : Role -> JE.Value
encodeRole r =
    JE.string (roleToString r)


decodeProjectMember : JD.Decoder ProjectMember
decodeProjectMember =
    JD.succeed ProjectMember
        |> andMap (JD.field "id" JD.int |> JD.map makeUserId)
        |> andMap (JD.field "name" JD.string)
        |> andMap (JD.field "role" decodeRole)


decodeUser : JD.Decoder User
decodeUser =
    JD.succeed User
        |> andMap (JD.field "id" JD.int |> JD.map makeUserId)
        |> andMap (JD.field "name" JD.string)


decodeProjectEdit : JD.Decoder ProjectEdit
decodeProjectEdit =
    JD.succeed ProjectEdit
        |> andMap (JD.field "project" decodeProject)
        |> andMap (JD.field "members" (JD.list decodeProjectMember))


encodeSaveProjectMember : SaveProjectMember -> JE.Value
encodeSaveProjectMember m =
    JE.object
        [ ( "id", JE.int (getUserIdVal m.id) )
        , ( "role", encodeRole m.role )
        , ( "delete", JE.bool m.delete )
        ]


encodeSaveProjectEdit : SaveProjectEdit -> JE.Value
encodeSaveProjectEdit p =
    JE.object
        [ ( "project", encodeSaveProject p.project )
        , ( "members", JE.list encodeSaveProjectMember p.members )
        ]


decodeSavedProjectEdit : JD.Decoder SavedProjectEdit
decodeSavedProjectEdit =
    JD.succeed SavedProjectEdit
        |> andMap (JD.field "project" decodeProject)
        |> andMap (JD.field "members" <| JD.list decodeProjectMember)


encodeSaveProjectTime : SaveProjectTime -> JE.Value
encodeSaveProjectTime t =
    JE.object
        [ ( "project", JE.int (getProjectIdVal t.project) )
        , ( "savetimeentries", JE.list encodeSaveTimeEntry t.savetimeentries )
        , ( "deletetimeentries", JE.list (getTimeEntryIdVal >> JE.int) t.deletetimeentries )
        , ( "savepayentries", JE.list encodeSavePayEntry t.savepayentries )
        , ( "deletepayentries", JE.list (getPayEntryIdVal >> JE.int) t.deletepayentries )
        , ( "saveallocations", JE.list encodeSaveAllocation t.saveallocations )
        , ( "deleteallocations", JE.list (getAllocationIdVal >> JE.int) t.deleteallocations )
        ]


decodeTimeEntry : JD.Decoder TimeEntry
decodeTimeEntry =
    JD.succeed TimeEntry
        |> andMap (JD.field "id" JD.int |> JD.map makeTimeEntryId)
        |> andMap (JD.field "project" JD.int |> JD.map makeProjectId)
        |> andMap (JD.field "user" JD.int |> JD.map makeUserId)
        |> andMap (JD.field "description" JD.string)
        |> andMap (JD.field "startdate" JD.int)
        |> andMap (JD.field "enddate" JD.int)
        |> andMap (JD.field "ignore" JD.bool)
        |> andMap (JD.field "createdate" JD.int)
        |> andMap (JD.field "changeddate" JD.int)
        |> andMap (JD.field "creator" JD.int |> JD.map makeUserId)


encodeSaveTimeEntry : SaveTimeEntry -> JE.Value
encodeSaveTimeEntry e =
    JE.object <|
        (e.id
            |> Maybe.map (\id -> (::) ( "id", JE.int (getTimeEntryIdVal id) ))
            |> Maybe.withDefault identity
        )
            [ ( "project", JE.int (getProjectIdVal e.project) )
            , ( "user", JE.int (getUserIdVal e.user) )
            , ( "description", JE.string e.description )
            , ( "startdate", JE.int e.startdate )
            , ( "enddate", JE.int e.enddate )
            , ( "ignore", JE.bool e.ignore )
            ]


decodePayEntry : JD.Decoder PayEntry
decodePayEntry =
    JD.succeed PayEntry
        |> andMap (JD.field "id" JD.int |> JD.map makePayEntryId)
        |> andMap (JD.field "project" JD.int |> JD.map makeProjectId)
        |> andMap (JD.field "user" JD.int |> JD.map makeUserId)
        |> andMap (JD.field "duration" JD.int)
        |> andMap (JD.field "paytype" decodePayType)
        |> andMap (JD.field "paymentdate" JD.int)
        |> andMap (JD.field "description" JD.string)
        |> andMap (JD.field "createdate" JD.int)
        |> andMap (JD.field "changeddate" JD.int)
        |> andMap (JD.field "creator" JD.int |> JD.map makeUserId)


encodeSavePayEntry : SavePayEntry -> JE.Value
encodeSavePayEntry e =
    JE.object <|
        (e.id
            |> Maybe.map (\id -> (::) ( "id", JE.int (getPayEntryIdVal id) ))
            |> Maybe.withDefault identity
        )
            [ ( "project", JE.int (getProjectIdVal e.project) )
            , ( "user", JE.int (getUserIdVal e.user) )
            , ( "duration", JE.int e.duration )
            , ( "paytype", encodePayType e.paytype )
            , ( "paymentdate", JE.int e.paymentdate )
            , ( "description", JE.string e.description )
            ]


decodeAllocation : JD.Decoder Allocation
decodeAllocation =
    JD.succeed Allocation
        |> andMap (JD.field "id" JD.int |> JD.map makeAllocationId)
        |> andMap (JD.field "project" JD.int |> JD.map makeProjectId)
        |> andMap (JD.field "duration" JD.int)
        |> andMap (JD.field "allocationdate" JD.int)
        |> andMap (JD.field "description" JD.string)
        |> andMap (JD.field "createdate" JD.int)
        |> andMap (JD.field "changeddate" JD.int)
        |> andMap (JD.field "creator" JD.int |> JD.map makeUserId)


encodeSaveAllocation : SaveAllocation -> JE.Value
encodeSaveAllocation e =
    JE.object <|
        (e.id
            |> Maybe.map (\id -> (::) ( "id", JE.int (getAllocationIdVal id) ))
            |> Maybe.withDefault identity
        )
            [ ( "project", JE.int (getProjectIdVal e.project) )
            , ( "duration", JE.int e.duration )
            , ( "allocationdate", JE.int e.allocationdate )
            , ( "description", JE.string e.description )
            ]


decodeProjectTime : JD.Decoder ProjectTime
decodeProjectTime =
    JD.succeed ProjectTime
        |> andMap (JD.field "project" <| decodeProject)
        |> andMap (JD.field "members" <| JD.list decodeProjectMember)
        |> andMap (JD.field "timeentries" <| JD.list decodeTimeEntry)
        |> andMap (JD.field "payentries" <| JD.list decodePayEntry)
        |> andMap (JD.field "allocations" <| JD.list decodeAllocation)
