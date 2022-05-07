module TimeclonkInterface exposing (SendMsg(..), ServerResponse(..), encodeEmail, encodeSendMsg, serverResponseDecoder, showServerResponse)

import Data exposing (UserId)
import Json.Decode as JD
import Json.Encode as JE



-- import Search as S


type SendMsg
    = GetProjectList UserId
    | GetProjectEdit Int
    | SaveProjectEdit Data.SaveProjectEdit
    | GetProjectTime Int
    | SaveProjectTime Data.SaveProjectTime
    | GetAllMembers


type ServerResponse
    = ServerError String
    | ProjectList (List Data.ListProject)
    | ProjectEdit Data.ProjectEdit
    | SavedProjectEdit Data.SavedProjectEdit
    | AllMembers (List Data.ProjectMember)
    | ProjectTime Data.ProjectTime


showServerResponse : ServerResponse -> String
showServerResponse sr =
    case sr of
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
        GetProjectList uid ->
            JE.object
                [ ( "what", JE.string "GetProjectList" )
                , ( "data", JE.int (Data.getUserIdVal uid) )
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