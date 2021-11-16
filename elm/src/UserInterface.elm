module UserInterface exposing (SendMsg(..), ServerResponse(..), encodeEmail, encodeSendMsg, serverResponseDecoder, showServerResponse)

import Data
import Json.Decode as JD
import Json.Encode as JE



-- import Search as S


type SendMsg
    = Register Data.Registration
    | Login Data.Login
    | ResetPassword Data.ResetPassword
    | SetPassword Data.SetPassword
    | Logout
    | ChangePassword Data.ChangePassword
    | ChangeEmail Data.ChangeEmail



-- | GetZkNote Int
-- | GetZkNoteEdit Data.GetZkNoteEdit
-- | GetZkNoteComments Data.GetZkNoteComments
-- | DeleteZkNote Int
-- | SaveZkNote Data.SaveZkNote
-- | SaveZkLinks Data.ZkLinks
-- | SaveZkNotePlusLinks Data.SaveZkNotePlusLinks
--   -- | GetZkLinks Data.GetZkLinks
-- | SearchZkNotes S.ZkNoteSearch
-- | SaveImportZkNotes (List Data.ImportZkNote)
-- | PowerDelete S.TagSearch
-- | SetHomeNote Int


type ServerResponse
    = RegistrationSent
    | UserExists
    | UnregisteredUser
    | InvalidUserOrPwd
    | NotLoggedIn
    | LoggedIn Data.LoginData
    | LoggedOut
    | ChangedPassword
    | ChangedEmail
    | ResetPasswordAck
    | SetPasswordAck
    | ServerError String



-- | ZkNoteSearchResult Data.ZkNoteSearchResult
-- | ZkListNoteSearchResult Data.ZkListNoteSearchResult
-- | SavedZkNotePlusLinks Data.SavedZkNote
-- | SavedZkNote Data.SavedZkNote
-- | DeletedZkNote Int
-- | ZkNote Data.ZkNote
-- | ZkNoteEdit Data.ZkNoteEdit
-- | ZkNoteComments (List Data.ZkNote)
-- | SavedZkLinks
-- | ZkLinks Data.ZkLinks
-- | SavedImportZkNotes
-- | PowerDeleteComplete Int
-- | HomeNoteSet Int


showServerResponse : ServerResponse -> String
showServerResponse sr =
    case sr of
        RegistrationSent ->
            "RegistrationSent"

        UserExists ->
            "UserExists"

        UnregisteredUser ->
            "UnregisteredUser"

        NotLoggedIn ->
            "NotLoggedIn"

        InvalidUserOrPwd ->
            "InvalidUserOrPwd"

        LoggedIn _ ->
            "LoggedIn"

        LoggedOut ->
            "LoggedOut"

        ResetPasswordAck ->
            "ResetPasswordAck"

        SetPasswordAck ->
            "SetPasswordAck"

        ChangedPassword ->
            "ChangedPassword"

        ChangedEmail ->
            "ChangedEmail"

        ServerError _ ->
            "ServerError"


encodeSendMsg : SendMsg -> JE.Value
encodeSendMsg sm =
    case sm of
        Register registration ->
            JE.object
                [ ( "what", JE.string "register" )
                , ( "data", Data.encodeRegistration registration )
                ]

        Login login ->
            JE.object
                [ ( "what", JE.string "login" )
                , ( "data", Data.encodeLogin login )
                ]

        Logout ->
            JE.object
                [ ( "what", JE.string "logout" )
                ]

        ResetPassword chpwd ->
            JE.object
                [ ( "what", JE.string "resetpassword" )
                , ( "data", Data.encodeResetPassword chpwd )
                ]

        SetPassword chpwd ->
            JE.object
                [ ( "what", JE.string "setpassword" )
                , ( "data", Data.encodeSetPassword chpwd )
                ]

        ChangePassword chpwd ->
            JE.object
                [ ( "what", JE.string "ChangePassword" )
                , ( "data", Data.encodeChangePassword chpwd )
                ]

        ChangeEmail chpwd ->
            JE.object
                [ ( "what", JE.string "ChangeEmail" )
                , ( "data", Data.encodeChangeEmail chpwd )
                ]


encodeEmail : String -> JE.Value
encodeEmail email =
    JE.object
        [ ( "email", JE.string email )
        ]


serverResponseDecoder : JD.Decoder ServerResponse
serverResponseDecoder =
    JD.at [ "what" ]
        JD.string
        |> JD.andThen
            (\what ->
                case what of
                    "registration sent" ->
                        JD.succeed RegistrationSent

                    "unregistered user" ->
                        JD.succeed UnregisteredUser

                    "user exists" ->
                        JD.succeed UserExists

                    "logged in" ->
                        JD.map LoggedIn (JD.at [ "content" ] Data.decodeLoginData)

                    "logged out" ->
                        JD.succeed LoggedOut

                    "not logged in" ->
                        JD.succeed NotLoggedIn

                    "invalid user or pwd" ->
                        JD.succeed InvalidUserOrPwd

                    "resetpasswordack" ->
                        JD.succeed ResetPasswordAck

                    "setpasswordack" ->
                        JD.succeed SetPasswordAck

                    "changed password" ->
                        JD.succeed ChangedPassword

                    "changed email" ->
                        JD.succeed ChangedEmail

                    "server error" ->
                        JD.map ServerError (JD.at [ "content" ] JD.string)

                    wat ->
                        JD.succeed
                            (ServerError ("invalid 'what' from server: " ++ wat))
            )
