module TimeclonkInterface exposing (SendMsg(..), ServerResponse(..), encodeEmail, encodeSendMsg, serverResponseDecoder, showServerResponse)

import Data
import Json.Decode as JD
import Json.Encode as JE
import Orgauth.Data as OD exposing (UserId, getUserIdVal, makeUserId)


type SendMsg
    = GetProjectList UserId
    | GetProjectEdit Int
    | SaveProjectEdit Data.SaveProjectEdit
    | GetProjectTime Int
    | SaveProjectTime Data.SaveProjectTime
    | GetUserTime
    | GetAllUsers


type ServerResponse
    = ServerError String
    | ProjectList (List Data.ListProject)
    | ProjectEdit Data.ProjectEdit
    | SavedProjectEdit Data.SavedProjectEdit
    | AllUsers (List Data.User)
    | ProjectTime Data.ProjectTime
    | UserTime (List Data.TimeEntry)
    | NotLoggedIn
    | InvalidUserOrPwd


showServerResponse : ServerResponse -> String
showServerResponse sr =
    case sr of
        ServerError e ->
            "ServerError: '" ++ e ++ "'"

        ProjectList _ ->
            "ProjectList"

        ProjectEdit _ ->
            "ProjectEdit"

        ProjectTime _ ->
            "ProjectTime"

        UserTime _ ->
            "UserTime"

        SavedProjectEdit _ ->
            "SavedProjectEdit"

        AllUsers _ ->
            "AllMembers"

        NotLoggedIn ->
            "NotLoggedIn"

        InvalidUserOrPwd ->
            "InvalidUserOrPwd"


encodeSendMsg : SendMsg -> JE.Value
encodeSendMsg sm =
    case sm of
        GetProjectList uid ->
            JE.object
                [ ( "what", JE.string "GetProjectList" )
                , ( "data", JE.int (getUserIdVal uid) )
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

        GetUserTime ->
            JE.object
                [ ( "what", JE.string "GetUserTime" )
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

        GetAllUsers ->
            JE.object
                [ ( "what", JE.string "GetAllUsers" )
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

                    "allusers" ->
                        JD.map AllUsers (JD.at [ "content" ] (JD.list Data.decodeUser))

                    "usertime" ->
                        JD.map UserTime (JD.at [ "content" ] (JD.list Data.decodeTimeEntry))

                    "not logged in" ->
                        JD.succeed NotLoggedIn

                    "invalid user or pwd" ->
                        JD.succeed InvalidUserOrPwd

                    wat ->
                        JD.succeed
                            (ServerError ("invalid 'what' from server: " ++ wat))
            )
