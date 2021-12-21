module Util exposing
    ( Size
    , Stopoid(..)
    , YMDMS
    , andMap
    , captchaQ
    , compareColor
    , deadEndToString
    , deadEndsToString
    , first
    , foldUntil
    , httpErrorString
    , isJust
    , leadingZeroInt
    , mapNothing
    , maxInt
    , mbl
    , mblist
    , minInt
    , monthInt
    , paramParser
    , paramsParser
    , parseTime
    , problemToString
    , rest
    , rslist
    , sameDay
    , showDateTime
    , showTime
    , splitAt
    , toTimeMonth
    , trueforany
    , truncateDots
    , ymdsParser
    )

import DateTime
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode exposing (Decoder, map2)
import ParseHelp exposing (listOf)
import Parser as P exposing ((|.), (|=), Parser, Problem(..), oneOf, succeed, symbol)
import Random exposing (Seed, int, step)
import TangoColors as Color
import Time


type alias Size =
    { width : Int
    , height : Int
    }


maxInt : Int
maxInt =
    9007199254740991


minInt : Int
minInt =
    -9007199254740991


compareColor : Element.Color -> Element.Color -> Order
compareColor l r =
    let
        lrgb =
            Element.toRgb l

        rrgb =
            Element.toRgb r
    in
    case compare lrgb.red rrgb.red of
        EQ ->
            case compare lrgb.green rrgb.green of
                EQ ->
                    case compare lrgb.blue rrgb.blue of
                        EQ ->
                            compare lrgb.alpha rrgb.alpha

                        b ->
                            b

                c ->
                    c

        a ->
            a


paramParser : P.Parser ( String, String )
paramParser =
    P.succeed (\a b -> ( a, b ))
        |= P.getChompedString
            (P.chompWhile (\c -> c /= '='))
        |. P.symbol "="
        |= P.getChompedString
            (P.chompWhile (\c -> c /= '&'))


paramsParser : P.Parser (Dict String String)
paramsParser =
    P.succeed (\a b -> Dict.fromList <| a :: b)
        |= paramParser
        |= listOf
            (P.succeed identity
                |. symbol "&"
                |= paramParser
            )


httpErrorString : Http.Error -> String
httpErrorString e =
    case e of
        Http.BadUrl str ->
            "badurl" ++ str

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "networkerror"

        Http.BadStatus x ->
            "badstatus: " ++ String.fromInt x

        Http.BadBody s ->
            "badbodyd\nstring: " ++ s


rest : List a -> List a
rest list =
    case List.tail list of
        Nothing ->
            []

        Just elts ->
            elts


first : (a -> Maybe b) -> List a -> Maybe b
first f l =
    case List.head l of
        Just e ->
            case f e of
                Just x ->
                    Just x

                Nothing ->
                    first f (rest l)

        Nothing ->
            Nothing


mapNothing : a -> Maybe a -> Maybe a
mapNothing aprime mba =
    case mba of
        Just a ->
            Just a

        Nothing ->
            Just aprime


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


trueforany : (a -> Bool) -> List a -> Bool
trueforany f l =
    case List.head l of
        Just e ->
            if f e then
                True

            else
                trueforany f (rest l)

        Nothing ->
            False


mblist : List (Maybe a) -> Maybe (List a)
mblist mbs =
    Maybe.map List.reverse <|
        List.foldl
            (\mba mblst ->
                case mblst of
                    Nothing ->
                        Nothing

                    Just lst ->
                        case mba of
                            Nothing ->
                                Nothing

                            Just a ->
                                Just <| a :: lst
            )
            (Just [])
            mbs


mbl : Maybe a -> List a
mbl mba =
    case mba of
        Just x ->
            [ x ]

        Nothing ->
            []


{-| de-result a list
-}
rslist : List (Result x a) -> Result x (List a)
rslist l =
    List.foldr
        (\rn rs ->
            rs
                |> Result.andThen (\ls -> Result.map (\n -> n :: ls) rn)
        )
        (Ok [])
        l


{-| a function passed to foldUntil must return this.
-}
type Stopoid b
    = Go b
    | Stop b


{-| keep folding until a condition is met, then stop.
-}
foldUntil : (a -> b -> Stopoid b) -> b -> List a -> b
foldUntil fn initb lst =
    case lst of
        [] ->
            initb

        fst :: rst ->
            case fn fst initb of
                Stop retb ->
                    retb

                Go updb ->
                    foldUntil fn updb rst


splitAt : (a -> Bool) -> List a -> ( List a, List a )
splitAt test list =
    case list of
        a :: b ->
            if test a then
                ( [], a :: b )

            else
                let
                    ( x, y ) =
                        splitAt test b
                in
                ( a :: x, y )

        [] ->
            ( [], [] )


monthInt : Time.Month -> Int
monthInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


showDateTime : Time.Zone -> Time.Posix -> String
showDateTime zone time =
    (String.fromInt <| Time.toYear zone time)
        ++ "/"
        ++ (Time.toMonth zone time
                |> monthInt
                |> String.fromInt
                |> String.padLeft 2 '0'
           )
        ++ "/"
        ++ (Time.toDay zone time
                |> String.fromInt
                |> String.padLeft 2 '0'
           )
        ++ " "
        ++ (Time.toHour zone time |> String.fromInt |> String.padLeft 2 '0')
        ++ ":"
        ++ (Time.toMinute zone time |> String.fromInt |> String.padLeft 2 '0')
        ++ ":"
        ++ (Time.toSecond zone time |> String.fromInt |> String.padLeft 2 '0')


showTime : Time.Zone -> Time.Posix -> String
showTime zone time =
    (Time.toHour zone time |> String.fromInt |> String.padLeft 2 '0')
        ++ ":"
        ++ (Time.toMinute zone time |> String.fromInt |> String.padLeft 2 '0')
        ++ ":"
        ++ (Time.toSecond zone time |> String.fromInt |> String.padLeft 2 '0')


sameDay : Time.Zone -> Time.Posix -> Time.Posix -> Bool
sameDay zone time1 time2 =
    (Time.toYear zone time1 == Time.toYear zone time2)
        && (Time.toMonth zone time1 == Time.toMonth zone time2)
        && (Time.toDay zone time1 == Time.toDay zone time2)


toTimeMonth : Int -> Time.Month
toTimeMonth monthnum =
    case monthnum of
        1 ->
            Time.Jan

        2 ->
            Time.Feb

        3 ->
            Time.Mar

        4 ->
            Time.Apr

        5 ->
            Time.May

        6 ->
            Time.Jun

        7 ->
            Time.Jul

        8 ->
            Time.Aug

        9 ->
            Time.Sep

        10 ->
            Time.Oct

        11 ->
            Time.Nov

        _ ->
            Time.Dec


parseTime : Time.Zone -> String -> Result (List P.DeadEnd) (Maybe Time.Posix)
parseTime zone string =
    P.run ymdsParser string
        |> Result.map
            (\x ->
                DateTime.fromRawParts
                    { year = x.year
                    , month = toTimeMonth x.month
                    , day = x.day
                    }
                    { hours = x.hour
                    , minutes = x.minute
                    , seconds = x.second
                    , milliseconds = 0
                    }
                    |> Maybe.map
                        (DateTime.toPosix
                            >> (\p ->
                                    Time.posixToMillis p
                                        - DateTime.getTimezoneOffset zone p
                                        |> Time.millisToPosix
                               )
                        )
            )


type alias YMDMS =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    , minute : Int
    , second : Int
    }


leadingZeroInt : Parser Int
leadingZeroInt =
    succeed identity
        |. P.chompWhile ((==) '0')
        |= oneOf [ P.int, succeed 0 ]


ymdsParser : Parser YMDMS
ymdsParser =
    succeed YMDMS
        |= P.int
        |. oneOf [ symbol "/", symbol "-" ]
        |= leadingZeroInt
        |. oneOf [ symbol "/", symbol "-" ]
        |= leadingZeroInt
        |. symbol " "
        |= leadingZeroInt
        |. symbol ":"
        |= leadingZeroInt
        |. symbol ":"
        |= leadingZeroInt


captchaQ : Seed -> ( Seed, String, Int )
captchaQ seed =
    let
        ( a, seed1 ) =
            step (int 0 100) seed

        ( b, seed2 ) =
            step (int 0 100) seed1
    in
    ( seed2
    , "Whats " ++ String.fromInt a ++ " + " ++ String.fromInt b ++ "?"
    , a + b
    )


truncateDots : String -> Int -> String
truncateDots str len =
    let
        l =
            String.length str
    in
    if l > len + 3 then
        String.left len str ++ "..."

    else
        str


deadEndsToString : List P.DeadEnd -> String
deadEndsToString deadEnds =
    String.concat (List.intersperse "; " (List.map deadEndToString deadEnds))


deadEndToString : P.DeadEnd -> String
deadEndToString deadend =
    problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


problemToString : P.Problem -> String
problemToString p =
    case p of
        Expecting s ->
            "expecting '" ++ s ++ "'"

        ExpectingInt ->
            "expecting int"

        ExpectingHex ->
            "expecting octal"

        ExpectingOctal ->
            "expecting octal"

        ExpectingBinary ->
            "expecting binary"

        ExpectingFloat ->
            "expecting float"

        ExpectingNumber ->
            "expecting number"

        ExpectingVariable ->
            "expecting variable"

        ExpectingSymbol s ->
            "expecting symbol '" ++ s ++ "'"

        ExpectingKeyword s ->
            "expecting keyword '" ++ s ++ "'"

        ExpectingEnd ->
            "expecting end"

        UnexpectedChar ->
            "unexpected char"

        Problem s ->
            "problem " ++ s

        BadRepeat ->
            "bad repeat"



{-

   andMap example.

   parseMX : Decoder MusicXml
   parseMX =
       succeed MusicXml
           |> andMap (maybe (stringAttr "version"))
           |> andMap (maybe (path [ "work", "work-title" ] (single string)))
           |> andMap (maybe (path [ "movement-title" ] (single string)))
           |> andMap (path [ "credit", "credit-words" ] (list string))
           |> andMap (maybe (path [ "identification", "creator" ] (single parseCreator)))
           |> andMap (path [ "part-list", "score-part" ] (list parsePlp))
           |> andMap (path [ "part" ] (list parsePart))


-}


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    map2 (|>)
