module Paginator exposing (Model, filter, init, onBack, onForward, onToEnd, onToStart, view)

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
    , toStartMsg : msg
    , forwardMsg : msg
    , backMsg : msg
    , toEndMsg : msg
    }


init : msg -> msg -> msg -> msg -> Int -> Int -> Model msg
init forwardMsg backMsg toStartMsg toEndMsg offset pageincrement =
    { offset = offset
    , pageincrement = pageincrement
    , forwardMsg = forwardMsg
    , backMsg = backMsg
    , toStartMsg = toStartMsg
    , toEndMsg = toEndMsg
    }


onForward : Model msg -> Model msg
onForward model =
    { model | offset = model.offset + model.pageincrement }


onBack : Model msg -> Model msg
onBack model =
    { model | offset = model.offset - model.pageincrement }


onToStart : Model msg -> Model msg
onToStart model =
    { model | offset = 0 }


onToEnd : Int -> Model msg -> Model msg
onToEnd itemcount model =
    { model | offset = max 0 (itemcount - model.pageincrement) }


filter : Model msg -> List a -> List a
filter model list =
    list
        |> List.drop model.offset
        |> List.take model.pageincrement


view : Int -> Model msg -> Element msg
view itemcount model =
    E.row [ E.spacing 8 ]
        [ if model.offset == 0 then
            EI.button
                Common.disabledButtonStyle
                { onPress = Nothing, label = E.text "|<" }

          else
            EI.button
                Common.buttonStyle
                { onPress = Just model.toStartMsg, label = E.text "|<" }
        , if model.offset == 0 then
            EI.button
                Common.disabledButtonStyle
                { onPress = Nothing, label = E.text "<" }

          else
            EI.button
                Common.buttonStyle
                { onPress = Just model.backMsg, label = E.text "<" }
        , if model.offset < itemcount - model.pageincrement then
            EI.button
                Common.buttonStyle
                { onPress = Just model.forwardMsg, label = E.text ">" }

          else
            EI.button
                Common.disabledButtonStyle
                { onPress = Nothing, label = E.text ">" }
        , if model.offset < itemcount - model.pageincrement then
            EI.button
                Common.buttonStyle
                { onPress = Just model.toEndMsg, label = E.text ">|" }

          else
            EI.button
                Common.disabledButtonStyle
                { onPress = Nothing, label = E.text ">|" }
        ]
