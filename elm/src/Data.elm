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
