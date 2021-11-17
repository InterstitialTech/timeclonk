module Data exposing (..)

import Json.Decode as JD
import Json.Encode as JE
import UUID exposing (UUID)
import Url.Builder as UB
import Util exposing (andMap)



----------------------------------------
-- types sent to or from the server.
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
    { userid : Int
    , name : String
    }



----------------------------------------
-- Timeclonk specific types.
----------------------------------------


type alias ListProject =
    { id : Int
    , name : String
    }


type alias Project =
    { id : Int
    , name : String
    , description : String
    , public : Bool
    , createdate : Int
    , changeddate : Int
    }


type alias SaveProject =
    { id : Maybe Int
    , name : String
    , description : String
    , public : Bool
    }


type alias SavedProject =
    { id : Int
    , changeddate : Int
    }


type alias ProjectMember =
    { id : Int
    , name : String
    }


type alias ProjectEdit =
    { project : Project
    , members : List ProjectMember
    }


type alias SaveProjectMember =
    { id : Int
    , delete : Bool
    }


type alias SaveProjectEdit =
    { project : SaveProject
    , members : List SaveProjectMember
    }


type alias SavedProjectEdit =
    { project : SavedProject
    , members : List ProjectMember
    }


type alias TimeEntry =
    { id : Int
    , project : Int
    , user : Int
    , description : String
    , startdate : Int
    , enddate : Int
    , createdate : Int
    , changeddate : Int
    , creator : Int
    }


type alias SaveTimeEntry =
    { id : Maybe Int
    , project : Int
    , user : Int
    , description : String
    , startdate : Int
    , enddate : Int
    }


type alias SaveProjectTime =
    { project : Int
    , savetimeentries : List SaveTimeEntry
    , deletetimeentries : List Int
    }


type alias ProjectTime =
    { project : Project
    , members : List ProjectMember
    , timeentries : List TimeEntry
    }



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
        |> andMap (JD.field "userid" JD.int)
        |> andMap (JD.field "name" JD.string)



----------------------------------------------------------------


decodeListProject : JD.Decoder ListProject
decodeListProject =
    JD.succeed ListProject
        |> andMap (JD.field "id" JD.int)
        |> andMap (JD.field "name" JD.string)


encodeSaveProject : SaveProject -> JE.Value
encodeSaveProject sp =
    JE.object <|
        [ ( "name", JE.string sp.name )
        , ( "description", JE.string sp.description )
        , ( "public", JE.bool sp.public )
        ]
            ++ (sp.id
                    |> Maybe.map (\id -> [ ( "id", JE.int id ) ])
                    |> Maybe.withDefault []
               )


decodeProject : JD.Decoder Project
decodeProject =
    JD.succeed Project
        |> andMap (JD.field "id" JD.int)
        |> andMap (JD.field "name" JD.string)
        |> andMap (JD.field "description" JD.string)
        |> andMap (JD.field "public" JD.bool)
        |> andMap (JD.field "createdate" JD.int)
        |> andMap (JD.field "changeddate" JD.int)


decodeSavedProject : JD.Decoder SavedProject
decodeSavedProject =
    JD.succeed SavedProject
        |> andMap (JD.field "id" JD.int)
        |> andMap (JD.field "changeddate" JD.int)


decodeProjectMember : JD.Decoder ProjectMember
decodeProjectMember =
    JD.succeed ProjectMember
        |> andMap (JD.field "id" JD.int)
        |> andMap (JD.field "name" JD.string)


decodeProjectEdit : JD.Decoder ProjectEdit
decodeProjectEdit =
    JD.succeed ProjectEdit
        |> andMap (JD.field "project" decodeProject)
        |> andMap (JD.field "members" (JD.list decodeProjectMember))


encodeSaveProjectMember : SaveProjectMember -> JE.Value
encodeSaveProjectMember m =
    JE.object
        [ ( "id", JE.int m.id )
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
        |> andMap (JD.field "project" decodeSavedProject)
        |> andMap (JD.field "members" <| JD.list decodeProjectMember)


encodeSaveProjectTime : SaveProjectTime -> JE.Value
encodeSaveProjectTime t =
    JE.object
        [ ( "project", JE.int t.project )
        , ( "savetimeentries", JE.list encodeSaveTimeEntry t.savetimeentries )
        , ( "deletetimeentries", JE.list JE.int t.deletetimeentries )
        ]


decodeTimeEntry : JD.Decoder TimeEntry
decodeTimeEntry =
    JD.succeed TimeEntry
        |> andMap (JD.field "id" JD.int)
        |> andMap (JD.field "project" JD.int)
        |> andMap (JD.field "user" JD.int)
        |> andMap (JD.field "description" JD.string)
        |> andMap (JD.field "startdate" JD.int)
        |> andMap (JD.field "enddate" JD.int)
        |> andMap (JD.field "createdate" JD.int)
        |> andMap (JD.field "changeddate" JD.int)
        |> andMap (JD.field "creator" JD.int)


encodeSaveTimeEntry : SaveTimeEntry -> JE.Value
encodeSaveTimeEntry e =
    JE.object <|
        (e.id
            |> Maybe.map (\id -> (::) ( "id", JE.int id ))
            |> Maybe.withDefault identity
        )
            [ ( "project", JE.int e.project )
            , ( "user", JE.int e.user )
            , ( "description", JE.string e.description )
            , ( "startdate", JE.int e.startdate )
            , ( "enddate", JE.int e.enddate )
            ]


decodeProjectTime : JD.Decoder ProjectTime
decodeProjectTime =
    JD.succeed ProjectTime
        |> andMap (JD.field "project" <| decodeProject)
        |> andMap (JD.field "members" <| JD.list decodeProjectMember)
        |> andMap (JD.field "timeentries" <| JD.list decodeTimeEntry)
