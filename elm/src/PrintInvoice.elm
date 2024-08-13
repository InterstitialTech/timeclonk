module PrintInvoice exposing (GDModel, Model, Msg(..), init, update, view)

import Data
import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Font as EF
import Element.Input as EI
import GenDialog as GD
import Orgauth.Data as Data
import Util


type alias Model =
    { date : String
    , duedate : String
    , sequence : Int
    , extravalues : Dict String String
    , printInvoiceInternal : Data.PrintInvoiceInternal
    }


type Msg
    = DateChanged String
    | DueDateChanged String
    | SequenceChanged String
    | OkClick
    | CancelClick
    | Noop


type alias GDModel =
    GD.Model Model Msg ( Data.PrintInvoice, Data.SaveProjectInvoice )


init : Data.PrintInvoiceInternal -> String -> List ( String, String ) -> List (E.Attribute Msg) -> Element () -> GDModel
init pi date extravalues buttonStyle underLay =
    { view = view buttonStyle
    , update = update
    , model =
        { date = date
        , duedate = ""
        , sequence = pi.seq + 1
        , extravalues = Dict.fromList extravalues
        , printInvoiceInternal = pi
        }
    , underLay = underLay
    }


view : List (E.Attribute Msg) -> Maybe Util.Size -> Model -> Element Msg
view buttonStyle mbsize model =
    E.column
        [ E.width (mbsize |> Maybe.map .width |> Maybe.withDefault 500 |> E.px)
        , E.height E.shrink
        , E.spacing 10
        ]
        [ E.el [ EF.size 20, EF.bold, E.centerX ] <| E.text "Print Invoice"
        , EI.text
            []
            { onChange =
                DateChanged
            , text = model.date
            , placeholder = Nothing
            , label =
                EI.labelLeft
                    []
                    (E.text "date")
            }
        , EI.text
            []
            { onChange =
                DueDateChanged
            , text = model.duedate
            , placeholder = Nothing
            , label =
                EI.labelLeft
                    []
                    (E.text "due date")
            }
        , EI.text
            []
            { onChange =
                SequenceChanged
            , text = String.fromInt model.sequence
            , placeholder = Nothing
            , label =
                EI.labelLeft
                    []
                    (E.text "invoice sequence number")
            }
        , E.row []
            [ E.text "invoice id: "
            , E.text (Data.makeInvoiceId model.printInvoiceInternal.idtemplate model.date model.sequence)
            ]
        , E.row [ E.width E.fill, E.spacing 10 ]
            [ EI.button buttonStyle
                { onPress = Just OkClick, label = E.text "Ok" }
            , EI.button
                buttonStyle
                { onPress = Just CancelClick, label = E.text "Cancel" }
            ]
        ]


update : Msg -> Model -> GD.Transition Model ( Data.PrintInvoice, Data.SaveProjectInvoice )
update msg model =
    case msg of
        DateChanged s ->
            GD.Dialog { model | date = s }

        DueDateChanged s ->
            GD.Dialog { model | duedate = s }

        SequenceChanged s ->
            case String.toInt s of
                Just i ->
                    GD.Dialog { model | sequence = i }

                Nothing ->
                    GD.Dialog model

        CancelClick ->
            GD.Cancel

        OkClick ->
            let
                mpii =
                    model.printInvoiceInternal

                mpiis =
                    { mpii | seq = model.sequence }
            in
            GD.Ok ( Data.toPi mpiis model.date model.duedate, Data.toSaveProjectInvoice mpiis )

        Noop ->
            GD.Dialog model
