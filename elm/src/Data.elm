module Data exposing
    ( Allocation
    , AllocationId
    , ChangeEmail
    , ChangePassword
    , ListProject
    , Login
    , LoginData
    , PayEntry
    , PayEntryId
    , Project
    , ProjectEdit
    , ProjectId
    , ProjectMember
    , ProjectTime
    , Registration
    , ResetPassword
    , SaveAllocation
    , SavePayEntry
    , SaveProject
    , SaveProjectEdit
    , SaveProjectMember
    , SaveProjectTime
    , SaveTimeEntry
    , SavedProject
    , SavedProjectEdit
    , SetPassword
    , TimeEntry
    , TimeEntryId
    , UserId
    , decodeListProject
    , decodeLoginData
    , decodePayEntry
    , decodeProject
    , decodeProjectEdit
    , decodeProjectMember
    , decodeProjectTime
    , decodeSavedProject
    , decodeSavedProjectEdit
    , decodeTimeEntry
    , encodeChangeEmail
    , encodeChangePassword
    , encodeLogin
    , encodeRegistration
    , encodeResetPassword
    , encodeSavePayEntry
    , encodeSaveProject
    , encodeSaveProjectEdit
    , encodeSaveProjectMember
    , encodeSaveProjectTime
    , encodeSaveTimeEntry
    , encodeSetPassword
    , getAllocationIdVal
    , getPayEntryIdVal
    , getProjectIdVal
    , getTimeEntryIdVal
    , getUserIdVal
    , makeAllocationId
    , makePayEntryId
    , makeProjectId
    , makeTimeEntryId
    , makeUserId
    )

import Json.Decode as JD
import Json.Encode as JE
import UUID exposing (UUID)
import Url.Builder as UB
import Util exposing (andMap)



----------------------------------------
-- user, password, registration etc.
----------------------------------------


type alias Registration =
    { uid : String
    , pwd : String
    , email : String
    }


type alias Login =
    { uid : String
    , pwd : String
    }


type alias ResetPassword =
    { uid : String
    }


type alias SetPassword =
    { uid : String
    , newpwd : String
    , reset_key : UUID
    }


type alias ChangePassword =
    { oldpwd : String
    , newpwd : String
    }


type alias ChangeEmail =
    { pwd : String
    , email : String
    }


type alias LoginData =
    { userid : UserId
    , name : String
    }



----------------------------------------
-- Timeclonk specific types.
----------------------------------------


type alias ListProject =
    { id : ProjectId
    , name : String
    }


type alias Project =
    { id : ProjectId
    , name : String
    , description : String
    , public : Bool
    , rate : Maybe Int
    , currency : Maybe String
    , createdate : Int
    , changeddate : Int
    }


type alias SaveProject =
    { id : Maybe ProjectId
    , name : String
    , description : String
    , public : Bool
    , rate : Maybe Int
    , currency : Maybe String
    }


type alias SavedProject =
    { id : ProjectId
    , changeddate : Int
    }


type alias ProjectMember =
    { id : UserId
    , name : String
    }


type alias ProjectEdit =
    { project : Project
    , members : List ProjectMember
    }


type alias SaveProjectMember =
    { id : UserId
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


type alias PayEntry =
    { id : PayEntryId
    , project : ProjectId
    , user : UserId
    , duration : Int
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



-------------------------------------------
-- Id types.  They're all ints underneath.
-------------------------------------------


type UserId
    = UserId Int


makeUserId : Int -> UserId
makeUserId i =
    UserId i


getUserIdVal : UserId -> Int
getUserIdVal uid =
    case uid of
        UserId i ->
            i


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



----------------------------------------
-- Json encoders/decoders
----------------------------------------


encodeRegistration : Registration -> JE.Value
encodeRegistration l =
    JE.object
        [ ( "uid", JE.string l.uid )
        , ( "pwd", JE.string l.pwd )
        , ( "email", JE.string l.email )
        ]


encodeLogin : Login -> JE.Value
encodeLogin l =
    JE.object
        [ ( "uid", JE.string l.uid )
        , ( "pwd", JE.string l.pwd )
        ]


encodeResetPassword : ResetPassword -> JE.Value
encodeResetPassword l =
    JE.object
        [ ( "uid", JE.string l.uid )
        ]


encodeSetPassword : SetPassword -> JE.Value
encodeSetPassword l =
    JE.object
        [ ( "uid", JE.string l.uid )
        , ( "newpwd", JE.string l.newpwd )
        , ( "reset_key", UUID.toValue l.reset_key )
        ]


encodeChangePassword : ChangePassword -> JE.Value
encodeChangePassword l =
    JE.object
        [ ( "oldpwd", JE.string l.oldpwd )
        , ( "newpwd", JE.string l.newpwd )
        ]


encodeChangeEmail : ChangeEmail -> JE.Value
encodeChangeEmail l =
    JE.object
        [ ( "pwd", JE.string l.pwd )
        , ( "email", JE.string l.email )
        ]


decodeLoginData : JD.Decoder LoginData
decodeLoginData =
    JD.succeed LoginData
        |> andMap (JD.field "userid" JD.int |> JD.map makeUserId)
        |> andMap (JD.field "name" JD.string)



----------------------------------------------------------------


decodeListProject : JD.Decoder ListProject
decodeListProject =
    JD.succeed ListProject
        |> andMap (JD.field "id" JD.int |> JD.map makeProjectId)
        |> andMap (JD.field "name" JD.string)


encodeSaveProject : SaveProject -> JE.Value
encodeSaveProject sp =
    JE.object <|
        [ ( "name", JE.string sp.name )
        , ( "description", JE.string sp.description )
        , ( "public", JE.bool sp.public )
        ]
            ++ (sp.rate
                    |> Maybe.map (\rate -> [ ( "rate", JE.int rate ) ])
                    |> Maybe.withDefault []
               )
            ++ (sp.currency
                    |> Maybe.map (\currency -> [ ( "currency", JE.string currency ) ])
                    |> Maybe.withDefault []
               )
            ++ (sp.id
                    |> Maybe.map (\id -> [ ( "id", JE.int (getProjectIdVal id) ) ])
                    |> Maybe.withDefault []
               )


decodeProject : JD.Decoder Project
decodeProject =
    JD.succeed Project
        |> andMap (JD.field "id" JD.int |> JD.map makeProjectId)
        |> andMap (JD.field "name" JD.string)
        |> andMap (JD.field "description" JD.string)
        |> andMap (JD.field "public" JD.bool)
        |> andMap (JD.field "rate" <| JD.maybe JD.int)
        |> andMap (JD.field "currency" <| JD.maybe JD.string)
        |> andMap (JD.field "createdate" JD.int)
        |> andMap (JD.field "changeddate" JD.int)


decodeSavedProject : JD.Decoder SavedProject
decodeSavedProject =
    JD.succeed SavedProject
        |> andMap (JD.field "id" JD.int |> JD.map makeProjectId)
        |> andMap (JD.field "changeddate" JD.int)


decodeProjectMember : JD.Decoder ProjectMember
decodeProjectMember =
    JD.succeed ProjectMember
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
