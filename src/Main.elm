module Main exposing (..)

import Browser
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import Iso8601
import Json.Decode as Decode
import Task
import Time


main : Program {} Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { statement : Statement StatementRow
    , allStatementColumns : List ( Int, ColumnType, List String )
    , statementHasHeaders : Bool
    , statementHeaders : Dict Int String
    }


type alias StatementRow =
    { payerOrReceiver : String
    , dateOfPayment : Time.Posix
    , valueOfPayment : Float
    , allOtherInfo : List ( String, String )
    }


init : {} -> ( Model, Cmd msg )
init _ =
    ( { statement = NotAsked
      , statementHasHeaders = False
      , allStatementColumns = []
      , statementHeaders = Dict.empty
      }
    , Cmd.none
    )


type Statement entity
    = NotAsked
    | Loading
    | DefiningColumns
    | Ready (Dict Int entity)


type ColumnType
    = CommonInfo String
    | Payer
    | Date
    | Value


translateColumnType : ColumnType -> String
translateColumnType columnType =
    case columnType of
        CommonInfo _ ->
            "info"

        Payer ->
            "payer"

        Date ->
            "date"

        Value ->
            "value"


columnTypeToString : ColumnType -> String
columnTypeToString columnType =
    case columnType of
        CommonInfo _ ->
            "commoninfo"

        Payer ->
            "payer"

        Date ->
            "date"

        Value ->
            "value"


columnTypeFromString : String -> ColumnType
columnTypeFromString columnType =
    case columnType of
        "commoninfo" ->
            CommonInfo ""

        "payer" ->
            Payer

        "date" ->
            Date

        "value" ->
            Value

        _ ->
            CommonInfo columnType


type Msg
    = FileHasHeadersChecked Bool
    | FileRequested
    | FileSelected File
    | FileLoaded String
    | ColumnsSubmitted
    | ColumnTypeSelected Int String
    | CommonInfoHeaderChanged Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileHasHeadersChecked fileHasHeader ->
            ( { model | statementHasHeaders = fileHasHeader }
            , Cmd.none
            )

        FileRequested ->
            ( model
            , Select.file [ "text/csv" ] FileSelected
            )

        FileSelected file ->
            ( { model | statement = Loading }
            , Task.perform FileLoaded (File.toString file)
            )

        FileLoaded fileContent ->
            let
                ( headers, columns ) =
                    toColumns fileContent model.statementHasHeaders
            in
            ( { model
                | statement = DefiningColumns
                , allStatementColumns = columns
                , statementHeaders = headers
              }
            , Cmd.none
            )

        ColumnsSubmitted ->
            ( { model
                | statement =
                    model.allStatementColumns
                        |> toReportData
                        |> toReport
                        |> Ready
              }
            , Cmd.none
            )

        ColumnTypeSelected selectedColumnIndex selectedColumnType ->
            ( { model
                | allStatementColumns =
                    List.map
                        (\( columnIndex, columnType, cells ) ->
                            if columnIndex == selectedColumnIndex then
                                ( columnIndex, columnTypeFromString selectedColumnType, cells )

                            else
                                ( columnIndex, columnType, cells )
                        )
                        model.allStatementColumns
              }
            , Cmd.none
            )

        CommonInfoHeaderChanged changedColumnIndex commonInfo ->
            ( { model
                | allStatementColumns =
                    List.map
                        (\( columnIndex, columnType, cells ) ->
                            if columnIndex == changedColumnIndex then
                                ( columnIndex, CommonInfo commonInfo, cells )

                            else
                                ( columnIndex, columnType, cells )
                        )
                        model.allStatementColumns
              }
            , Cmd.none
            )


toReport :
    { payers : Dict Int String
    , values : Dict Int Float
    , dates : Dict Int Time.Posix
    , commonInfo : Dict Int (List ( String, String ))
    }
    -> Dict Int StatementRow
toReport { payers, dates, values, commonInfo } =
    payers
        |> Dict.foldr
            (\k payer acc ->
                Maybe.map2
                    (\d v ->
                        Dict.insert k
                            { payerOrReceiver = payer
                            , dateOfPayment = d
                            , valueOfPayment = v
                            , allOtherInfo =
                                Dict.get k commonInfo
                                    |> Maybe.withDefault []
                            }
                            acc
                    )
                    (Dict.get k dates)
                    (Dict.get k values)
                    |> Maybe.withDefault acc
            )
            Dict.empty


toReportData :
    List ( Int, ColumnType, List String )
    ->
        { payers : Dict Int String
        , values : Dict Int Float
        , dates : Dict Int Time.Posix
        , commonInfo : Dict Int (List ( String, String ))
        }
toReportData =
    List.foldr
        (\( _, columnType, cells ) acc ->
            cells
                |> List.indexedMap
                    (\index cell ->
                        ( index, cell )
                    )
                |> List.foldr
                    (\( index, cell ) stuffs ->
                        case columnType of
                            CommonInfo info ->
                                { stuffs
                                    | commonInfo =
                                        Dict.insert index
                                            (case Dict.get index stuffs.commonInfo of
                                                Just commonInfos ->
                                                    ( info, cell ) :: commonInfos

                                                Nothing ->
                                                    [ ( info, cell ) ]
                                            )
                                            stuffs.commonInfo
                                }

                            Payer ->
                                { stuffs
                                    | payers =
                                        if String.isEmpty cell then
                                            stuffs.payers

                                        else
                                            Dict.insert index
                                                cell
                                                stuffs.payers
                                }

                            Date ->
                                { stuffs
                                    | dates =
                                        (Iso8601.toTime
                                            (Decode.decodeString Decode.string cell
                                                |> Result.toMaybe
                                                |> Maybe.map
                                                    (\cleanCell ->
                                                        String.split "." cleanCell
                                                            |> List.reverse
                                                            |> String.join "-"
                                                    )
                                                |> Maybe.withDefault ""
                                            )
                                            |> Result.toMaybe
                                        )
                                            |> Maybe.map
                                                (\bitcherzz ->
                                                    Dict.insert index
                                                        bitcherzz
                                                        stuffs.dates
                                                )
                                            |> Maybe.withDefault stuffs.dates
                                }

                            Value ->
                                { stuffs
                                    | values =
                                        Decode.decodeString Decode.string cell
                                            |> Result.toMaybe
                                            |> Maybe.andThen
                                                (\cleanCell ->
                                                    String.replace "," "." cleanCell
                                                        |> String.toFloat
                                                        |> Maybe.map
                                                            (\flt ->
                                                                Dict.insert index
                                                                    flt
                                                                    stuffs.values
                                                            )
                                                )
                                            |> Maybe.withDefault stuffs.values
                                }
                    )
                    acc
        )
        { payers = Dict.empty
        , values = Dict.empty
        , dates = Dict.empty
        , commonInfo = Dict.empty
        }


toColumns : String -> Bool -> ( Dict Int String, List ( Int, ColumnType, List String ) )
toColumns stringReport fileHasHeaders =
    let
        maybeHeaders : Maybe (Dict Int String)
        maybeHeaders =
            if fileHasHeaders then
                List.head
                    (stringReport
                        |> String.split "\u{000D}\n"
                    )
                    |> Maybe.map
                        (\headersRow ->
                            String.split ";" headersRow
                                |> List.indexedMap (\index header -> ( index, header ))
                                |> Dict.fromList
                        )

            else
                Nothing

        rawRows : List String
        rawRows =
            if fileHasHeaders then
                List.tail
                    (stringReport
                        |> String.split "\u{000D}\n"
                    )
                    |> Maybe.withDefault []

            else
                stringReport
                    |> String.split "\u{000D}\n"

        columns_ : List ( Int, ColumnType, List String )
        columns_ =
            rawRows
                |> List.map (String.split ";")
                |> List.map
                    (List.indexedMap
                        (\index cell ->
                            ( index, cell )
                        )
                    )
                |> List.foldr
                    (\row acc ->
                        List.foldr
                            (\( index, cell ) columns ->
                                let
                                    column : List String
                                    column =
                                        Dict.get index columns
                                            |> Maybe.map (\cells -> cell :: cells)
                                            |> Maybe.withDefault (List.singleton cell)
                                in
                                Dict.insert index column columns
                            )
                            acc
                            row
                    )
                    Dict.empty
                |> Dict.toList
                |> List.map
                    (\( index, v ) ->
                        maybeHeaders
                            |> Maybe.andThen
                                (\headers ->
                                    Dict.get index headers
                                        |> Maybe.map (\header -> ( index, CommonInfo header, v ))
                                )
                            |> Maybe.withDefault ( index, CommonInfo (String.fromInt index), v )
                    )
                |> List.filter (\( _, _, cells ) -> List.all (String.isEmpty >> not) cells)
    in
    ( maybeHeaders |> Maybe.withDefault Dict.empty, columns_ )


view : Model -> Browser.Document Msg
view ({ statement } as model) =
    { title = "everyday life management"
    , body =
        [ case statement of
            NotAsked ->
                Html.div [ Attrs.class "flex justify-between" ]
                    [ Html.div []
                        [ Html.input
                            [ Attrs.type_ "button"
                            , Attrs.value "upload file"
                            , Attrs.class "p-2 m-2 border rounded"
                            , Events.onClick FileRequested
                            ]
                            []
                        , Html.div
                            []
                            [ Html.label [] [ Html.text "Does file have headers?" ]
                            , Html.input
                                [ Attrs.type_ "checkbox"
                                , Attrs.class "p-2 m-2 border rounded"
                                , Events.onCheck FileHasHeadersChecked
                                ]
                                []
                            ]
                        ]
                    ]

            Loading ->
                Html.div [] [ Html.text "loading" ]

            DefiningColumns ->
                Html.div [ Attrs.class "w-3/4 flex gap-8 flex-wrap justify-center" ]
                    ((model.allStatementColumns
                        |> List.map
                            (\( index, columnType, v ) ->
                                Html.div [ Attrs.class "flex flex-col w-64 gap-2 border rounded p-4" ]
                                    (Html.div [ Attrs.class "flex flex-row gap-4 justify-around" ]
                                        [ viewColumnTypeSelect columnType
                                            index
                                            (Dict.get index model.statementHeaders
                                                |> Maybe.withDefault ""
                                            )
                                        ]
                                        :: (List.take 3 v
                                                |> List.map
                                                    (\cell ->
                                                        Html.p [ Attrs.class "truncate" ]
                                                            [ Html.text cell ]
                                                    )
                                           )
                                    )
                            )
                     )
                        ++ [ Html.input [ Attrs.type_ "button", Attrs.value "submit", Events.onClick ColumnsSubmitted ] [] ]
                    )

            Ready report ->
                Html.div [ Attrs.class "flex flex-col w-full" ]
                    (Dict.toList report
                        |> List.sortBy (Tuple.second >> .dateOfPayment >> Time.posixToMillis)
                        |> List.map
                            (\( index, reportRow ) ->
                                Html.div [ Attrs.class "flex flex-col justify-center items-center" ]
                                    [ Html.div [ Attrs.class "flex flex-row gap-4 w-3/4 border-b" ]
                                        [ Html.p [ Attrs.class "w-1/3 p-2" ] [ Html.text reportRow.payerOrReceiver ]
                                        , Html.p [ Attrs.class "w-1/3 p-2 border-l" ]
                                            [ Html.text
                                                (Iso8601.fromTime reportRow.dateOfPayment
                                                    |> String.split "T"
                                                    |> List.head
                                                    |> Maybe.withDefault ""
                                                )
                                            ]
                                        , Html.p [ Attrs.class "w-1/3 p-2 border-l" ] [ Html.text (String.fromFloat reportRow.valueOfPayment) ]
                                        ]
                                    ]
                            )
                    )
        ]
    }


viewColumnTypeSelect : ColumnType -> Int -> String -> Html.Html Msg
viewColumnTypeSelect columnType index header =
    Html.div []
        [ case columnType of
            CommonInfo info ->
                Html.span []
                    [ Html.input
                        [ Attrs.type_ "text"
                        , Attrs.value info
                        , Events.onInput (CommonInfoHeaderChanged index)
                        ]
                        []
                    , viewTypeSelect index header
                    ]

            Payer ->
                Html.span []
                    [ Html.label [] [ Html.text "payer" ]
                    , viewTypeSelect index header
                    ]

            Date ->
                Html.span []
                    [ Html.label [] [ Html.text "date" ]
                    , viewTypeSelect index header
                    ]

            Value ->
                Html.span []
                    [ Html.label [] [ Html.text "value" ]
                    , viewTypeSelect index header
                    ]
        ]


viewTypeSelect : Int -> String -> Html.Html Msg
viewTypeSelect index header =
    Html.select [ Events.onInput (ColumnTypeSelected index) ]
        [ Html.option
            [ Attrs.value (columnTypeToString (CommonInfo header)) ]
            [ Html.text (translateColumnType (CommonInfo header)) ]
        , Html.option
            [ Attrs.value (columnTypeToString Payer) ]
            [ Html.text (translateColumnType Payer) ]
        , Html.option
            [ Attrs.value (columnTypeToString Date) ]
            [ Html.text (translateColumnType Date) ]
        , Html.option
            [ Attrs.value (columnTypeToString Value) ]
            [ Html.text (translateColumnType Value) ]
        ]
