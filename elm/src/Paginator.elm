module Paginator exposing (Model, WhichEnd(..), filter, init, onBack, onForward, onToEnd, onToStart, view)

import Common
import Element as E exposing (Element)
import Element.Background as EBk
import Element.Border as EBd
import Element.Events as EE
import Element.Font as EF
import Element.Input as EI
import Element.Keyed as EK
import Element.Region


type alias Model msg =
    { offset : Int
    , pageincrement : Int
    , atEnd : Bool
    , toStartMsg : msg
    , forwardMsg : msg
    , backMsg : Int -> msg
    , toEndMsg : Int -> msg
    }


type WhichEnd
    = Start
    | End


init : msg -> (Int -> msg) -> msg -> (Int -> msg) -> WhichEnd -> Int -> Model msg
init forwardMsg backMsg toStartMsg toEndMsg whichend pageincrement =
    { offset = 0
    , pageincrement = pageincrement
    , atEnd =
        case whichend of
            Start ->
                False

            End ->
                True
    , forwardMsg = forwardMsg
    , backMsg = backMsg
    , toStartMsg = toStartMsg
    , toEndMsg = toEndMsg
    }


onForward : Model msg -> Model msg
onForward model =
    if model.atEnd then
        model

    else
        { model | offset = model.offset + model.pageincrement, atEnd = False }


onBack : Int -> Model msg -> Model msg
onBack itemcount model =
    if model.atEnd then
        { model | offset = max 0 (itemcount - (2 * model.pageincrement)), atEnd = False }

    else
        { model | offset = max 0 <| model.offset - model.pageincrement, atEnd = False }


onToStart : Model msg -> Model msg
onToStart model =
    { model | offset = 0, atEnd = False }


onToEnd : Int -> Model msg -> Model msg
onToEnd itemcount model =
    { model | offset = max 0 (itemcount - model.pageincrement), atEnd = True }


filter : Model msg -> List a -> List a
filter model list =
    if model.atEnd then
        list
            |> List.reverse
            |> List.take model.pageincrement
            |> List.reverse

    else
        list
            |> List.drop model.offset
            |> List.take model.pageincrement


view : Int -> Model msg -> Element msg
view itemcount model =
    let
        bd =
            model.offset == 0 && (not model.atEnd || itemcount <= model.pageincrement)

        fe =
            model.offset < itemcount - model.pageincrement && not model.atEnd
    in
    E.row [ E.spacing 8 ]
        [ if bd then
            EI.button
                Common.disabledButtonStyle
                { onPress = Nothing, label = E.text "|<" }

          else
            EI.button
                Common.buttonStyle
                { onPress = Just model.toStartMsg, label = E.text "|<" }
        , if bd then
            EI.button
                Common.disabledButtonStyle
                { onPress = Nothing, label = E.text "<" }

          else
            EI.button
                Common.buttonStyle
                { onPress = Just (model.backMsg itemcount), label = E.text "<" }
        , if fe then
            EI.button
                Common.buttonStyle
                { onPress = Just model.forwardMsg, label = E.text ">" }

          else
            EI.button
                Common.disabledButtonStyle
                { onPress = Nothing, label = E.text ">" }
        , if fe then
            EI.button
                Common.buttonStyle
                { onPress = Just (model.toEndMsg itemcount), label = E.text ">|" }

          else
            EI.button
                Common.disabledButtonStyle
                { onPress = Nothing, label = E.text ">|" }
        ]
