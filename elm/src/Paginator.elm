module Paginator exposing (Model, view)

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
    , forwardMsg : msg
    , backMsg : msg
    }


init : msg -> msg -> Int -> Int -> Model msg
init forwardMsg backMsg offset pageincrement =
    { offset = offset
    , pageincrement = pageincrement
    , forwardMsg = forwardMsg
    , backMsg = backMsg
    }


onForward : Model msg -> Model msg
onForward model =
    { model | offset = model.offset + model.pageincrement }


onBack : Model msg -> Model msg
onBack model =
    { model | offset = model.offset - model.pageincrement }


view : Int -> Model msg -> Element msg
view itemcount model =
    E.row []
        [ if model.offset == 0 then
            EI.button
                Common.disabledButtonStyle
                { onPress = Nothing, label = E.text ">" }

          else
            EI.button
                Common.buttonStyle
                { onPress = Just model.forwardMsg, label = E.text ">" }
        , if model.offset == 0 then
            EI.button
                Common.disabledButtonStyle
                { onPress = Nothing, label = E.text "<" }

          else
            EI.button
                Common.buttonStyle
                { onPress = Just model.backMsg, label = E.text "<" }
        ]
