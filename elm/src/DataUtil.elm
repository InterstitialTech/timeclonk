module DataUtil exposing (..)

import Json.Encode as JE
import Orgauth.Data as OD exposing (UserId)
import TcProtocol exposing (ExtraField, InvoiceItem, PrintInvoice, ProjectId(..), ProjectMember, Role(..), SaveProjectInvoice, TimeClonkError(..), User, roleEncoder)
import Time
import Util


type alias LoginData =
    { userid : UserId
    , uuid : String
    , name : String
    , email : String
    , admin : Bool
    , active : Bool
    }


ldToOdLd : LoginData -> OD.LoginData
ldToOdLd ld =
    { userid = ld.userid
    , uuid = ld.uuid
    , name = ld.name
    , email = ld.email
    , admin = ld.admin
    , active = ld.active
    , remoteUrl = Nothing
    , data = Nothing
    }


odLdToLd : OD.LoginData -> LoginData
odLdToLd ld =
    { userid = ld.userid
    , uuid = ld.uuid
    , name = ld.name
    , email = ld.email
    , admin = ld.admin
    , active = ld.active
    }


showTimeClonkError : TimeClonkError -> String
showTimeClonkError tce =
    case tce of
        TeNotLoggedIn ->
            "not logged in"

        TeInvalidLogin ->
            "invalid login"

        TeOther s ->
            "server error: " ++ s


type alias UserInviteProject =
    { id : ProjectId, role : Role }


encodeUserInviteProject : UserInviteProject -> JE.Value
encodeUserInviteProject p =
    JE.object
        [ ( "id", TcProtocol.projectIdEncoder p.id )
        , ( "role", roleEncoder p.role )
        ]


type alias UserInviteData =
    { projects : List UserInviteProject }


encodeUserInviteData : UserInviteData -> JE.Value
encodeUserInviteData d =
    JE.object
        [ ( "projects", JE.list encodeUserInviteProject d.projects )
        ]


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


type alias PrintInvoiceInternal =
    { projectid : ProjectId
    , seq : Int
    , duedays : Maybe Int
    , idtemplate : String
    , payer : String
    , payee : String
    , items : List InvoiceItem
    , extraFields : List ExtraField
    , currency : Maybe String
    }


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
    , dueDate =
        if duedate == "" then
            Nothing

        else
            Just duedate
    , extraFields = pii.extraFields
    , currency = Maybe.withDefault "$" pii.currency
    }


toSaveProjectInvoice : PrintInvoiceInternal -> SaveProjectInvoice
toSaveProjectInvoice pi =
    { id = pi.projectid
    , extraFields = pi.extraFields
    , invoiceSeq = pi.seq
    }



-------------------------------------------
-- Id types.  They're all ints underneath.
-------------------------------------------


makeProjectId : Int -> ProjectId
makeProjectId i =
    Pid i


getProjectIdVal : ProjectId -> Int
getProjectIdVal uid =
    case uid of
        Pid i ->
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
