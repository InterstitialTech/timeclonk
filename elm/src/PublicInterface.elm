module PublicInterface exposing
    ( SendMsg(..)
    , ServerResponse(..)
    , encodeSendMsg
    , serverResponseDecoder
    , showServerResponse
    )

import Data
import Json.Decode as JD
import Json.Encode as JE
import Orgauth.Data as OD exposing (UserId, getUserIdVal, makeUserId)


type SendMsg
    = GetProjectTime Int


type ServerResponse
    = ServerError String
    | ProjectTime Data.ProjectTime


showServerResponse : ServerResponse -> String
showServerResponse sr =
    case sr of
        ServerError _ ->
            "ServerError"

        ProjectTime _ ->
            "ProjectTime"


encodeSendMsg : SendMsg -> JE.Value
encodeSendMsg sm =
    case sm of
        GetProjectTime pid ->
            JE.object
                [ ( "what", JE.string "GetProjectTime" )
                , ( "data", JE.int pid )
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

                    "projecttime" ->
                        JD.map ProjectTime (JD.at [ "content" ] Data.decodeProjectTime)

                    wat ->
                        JD.succeed
                            (ServerError ("invalid 'what' from server: " ++ wat))
            )
