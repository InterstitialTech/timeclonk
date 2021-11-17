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
    | GetProjectList Int
    | GetProjectEdit Int
    | SaveProjectEdit Data.SaveProjectEdit
    | GetProjectTime Int
    | SaveProjectTime Data.SaveProjectTime
    | GetAllMembers



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
    | ProjectList (List Data.ListProject)
    | ProjectEdit Data.ProjectEdit
    | SavedProjectEdit Data.SavedProjectEdit
    | AllMembers (List Data.ProjectMember)
    | ProjectTime Data.ProjectTime



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

        ProjectList _ ->
            "ProjectList"

        ProjectEdit _ ->
            "ProjectEdit"

        ProjectTime _ ->
            "ProjectTime"

        SavedProjectEdit _ ->
            "SavedProjectEdit"

        AllMembers _ ->
            "AllMembers"


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

        GetProjectList uid ->
            JE.object
                [ ( "what", JE.string "GetProjectList" )
                , ( "data", JE.int uid )
                ]

        GetProjectEdit pid ->
            JE.object
                [ ( "what", JE.string "GetProjectEdit" )
                , ( "data", JE.int pid )
                ]

        GetProjectTime pid ->
            JE.object
                [ ( "what", JE.string "GetProjectTime" )
                , ( "data", JE.int pid )
                ]

        SaveProjectEdit p ->
            JE.object
                [ ( "what", JE.string "SaveProjectEdit" )
                , ( "data", Data.encodeSaveProjectEdit p )
                ]

        SaveProjectTime p ->
            JE.object
                [ ( "what", JE.string "SaveProjectTime" )
                , ( "data", Data.encodeSaveProjectTime p )
                ]

        GetAllMembers ->
            JE.object
                [ ( "what", JE.string "GetAllMembers" )
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

                    "projectlist" ->
                        JD.map ProjectList (JD.at [ "content" ] (JD.list Data.decodeListProject))

                    "savedprojectedit" ->
                        JD.map SavedProjectEdit (JD.at [ "content" ] Data.decodeSavedProjectEdit)

                    "projectedit" ->
                        JD.map ProjectEdit (JD.at [ "content" ] Data.decodeProjectEdit)

                    "projecttime" ->
                        JD.map ProjectTime (JD.at [ "content" ] Data.decodeProjectTime)

                    "allmembers" ->
                        JD.map AllMembers (JD.at [ "content" ] (JD.list Data.decodeProjectMember))

                    wat ->
                        JD.succeed
                            (ServerError ("invalid 'what' from server: " ++ wat))
            )
