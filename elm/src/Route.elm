module Route exposing (Route(..), parseUrl, routeTitle, routeUrl)

import UUID exposing (UUID)
import Url exposing (Url)
import Url.Builder as UB
import Url.Parser as UP exposing ((</>))


type Route
    = LoginR
      -- | PublicZkNote Int
      -- | PublicZkPubId String
      -- | EditZkNoteR Int
      -- | EditZkNoteNew
    | ProjectEditR Int
    | ProjectTimeR Int String
    | ResetPasswordR String UUID
    | SettingsR
    | Top


routeTitle : String -> Route -> String
routeTitle appname route =
    case route of
        LoginR ->
            "login"

        ProjectEditR id ->
            "project edit " ++ String.fromInt id

        ProjectTimeR id mode ->
            "project time " ++ String.fromInt id ++ " " ++ mode

        -- PublicZkNote id ->
        --     "zknote " ++ String.fromInt id
        -- PublicZkPubId id ->
        --     id ++ " - zknotes"
        -- EditZkNoteR id ->
        --     "zknote " ++ String.fromInt id
        -- EditZkNoteNew ->
        --     "new zknote"
        ResetPasswordR _ _ ->
            "password reset"

        SettingsR ->
            "user settings"

        Top ->
            appname


parseUrl : Url -> Maybe Route
parseUrl url =
    UP.parse
        (UP.oneOf
            [ UP.map LoginR <|
                UP.s
                    "login"

            -- , UP.map PublicZkNote <|
            --     UP.s
            --         "note"
            --         </> UP.int
            -- , UP.map (\i -> PublicZkPubId (Maybe.withDefault "" (Url.percentDecode i))) <|
            --     UP.s
            --         "page"
            --         </> UP.string
            -- , UP.map EditZkNoteR <|
            --     UP.s
            --         "editnote"
            --         </> UP.int
            -- , UP.map EditZkNoteNew <|
            --     UP.s
            --         "editnote"
            --         </> UP.s "new"
            , UP.map ResetPasswordR <|
                UP.s
                    "reset"
                    </> UP.string
                    </> UP.custom "UUID" (UUID.fromString >> Result.toMaybe)
            , UP.map SettingsR <|
                UP.s
                    "settings"
            , UP.map ProjectEditR <|
                UP.s
                    "projectedit"
                    </> UP.int
            , UP.map ProjectTimeR <|
                UP.s
                    "projecttime"
                    </> UP.int
                    </> UP.string
            , UP.map Top <| UP.top
            ]
        )
        url


routeUrl : Route -> String
routeUrl route =
    case route of
        LoginR ->
            UB.absolute [ "login" ] []

        -- PublicZkNote id ->
        --     UB.absolute [ "note", String.fromInt id ] []
        -- PublicZkPubId pubid ->
        --     UB.absolute [ "page", pubid ] []
        -- EditZkNoteR id ->
        --     UB.absolute [ "editnote", String.fromInt id ] []
        -- EditZkNoteNew ->
        --     UB.absolute [ "editnote", "new" ] []
        ResetPasswordR user key ->
            UB.absolute [ "reset", user, UUID.toString key ] []

        SettingsR ->
            UB.absolute [ "settings" ] []

        Top ->
            UB.absolute [] []

        ProjectEditR id ->
            UB.absolute [ "projectedit", String.fromInt id ] []

        ProjectTimeR id mode ->
            UB.absolute [ "projecttime", String.fromInt id, mode ] []
