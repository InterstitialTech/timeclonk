module PrintInvoice exposing (GDModel, Model, Msg(..), init, update, view)

import Data
import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Background as EBk
import Element.Border as EBd
import Element.Font as EF
import Element.Input as EI
import GenDialog as GD
import Orgauth.Data as Data
import TcCommon as TC
import Util


type alias Model =
    { date : String
    , duedate : String
    , sequence : Int
    , extraFields : List ( String, String )
    , printInvoiceInternal : Data.PrintInvoiceInternal
    }


type Msg
    = DateChanged String
    | DueDateChanged String
    | SequenceChanged String
    | NameChanged Int String
    | ValueChanged Int String
    | AddItem
    | RemoveItem Int
    | OkClick
    | CancelClick
    | Noop


type alias GDModel =
    GD.Model Model Msg ( Data.PrintInvoice, Data.SaveProjectInvoice )


init : Data.PrintInvoiceInternal -> String -> List (E.Attribute Msg) -> Element () -> GDModel
init pi date buttonStyle underLay =
    { view = view buttonStyle
    , update = update
    , model =
        { date = date
        , duedate = ""
        , sequence = pi.seq + 1
        , extraFields = pi.extraFields
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
        , E.row []
            [ E.text "invoice id: "
            , E.el [ EF.bold ] <| E.text (Data.makeInvoiceId model.printInvoiceInternal.idtemplate model.date model.sequence)
            ]
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
        , E.column [ E.spacing TC.defaultSpacing, E.padding TC.defaultSpacing, EBd.width 1, E.width E.fill ]
            [ E.el [ EF.bold ] <| E.text "extra values"
            , E.table []
                { data = List.indexedMap (\i ( a, b ) -> ( i, a, b )) model.extraFields
                , columns =
                    [ { header = E.text "Name"
                      , width = E.fill
                      , view =
                            \( i, n, _ ) ->
                                EI.text
                                    []
                                    { onChange = NameChanged i
                                    , text = n
                                    , placeholder = Nothing
                                    , label = EI.labelHidden "name"
                                    }
                      }
                    , { header = E.text "Value"
                      , width = E.fill
                      , view =
                            \( i, _, v ) ->
                                EI.text
                                    []
                                    { onChange = ValueChanged i
                                    , text = v
                                    , placeholder = Nothing
                                    , label = EI.labelHidden "value"
                                    }
                      }
                    , { header = E.text "Delete"
                      , width = E.shrink
                      , view =
                            \( i, _, _ ) ->
                                EI.button
                                    buttonStyle
                                    { onPress = Just (RemoveItem i), label = E.text "x" }
                      }
                    ]
                }
            , E.row []
                [ EI.button
                    buttonStyle
                    { onPress = Just AddItem, label = E.text "Add Item" }
                ]
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

        NameChanged idx s ->
            GD.Dialog
                { model
                    | extraFields =
                        List.indexedMap
                            (\i ( n, v ) ->
                                if i == idx then
                                    ( s, v )

                                else
                                    ( n, v )
                            )
                            model.extraFields
                }

        ValueChanged idx s ->
            GD.Dialog
                { model
                    | extraFields =
                        List.indexedMap
                            (\i ( n, v ) ->
                                if i == idx then
                                    ( n, s )

                                else
                                    ( n, v )
                            )
                            model.extraFields
                }

        AddItem ->
            GD.Dialog { model | extraFields = List.append model.extraFields [ ( "", "" ) ] }

        RemoveItem idx ->
            GD.Dialog
                { model
                    | extraFields =
                        model.extraFields
                            |> List.indexedMap (\i v -> ( i, v ))
                            |> List.filterMap
                                (\( i, v ) ->
                                    if i == idx then
                                        Nothing

                                    else
                                        Just v
                                )
                }

        CancelClick ->
            GD.Cancel

        OkClick ->
            let
                mpii =
                    model.printInvoiceInternal

                mpiis =
                    { mpii | seq = model.sequence, extraFields = model.extraFields }
            in
            GD.Ok ( Data.toPi mpiis model.date model.duedate, Data.toSaveProjectInvoice mpiis )

        Noop ->
            GD.Dialog model
