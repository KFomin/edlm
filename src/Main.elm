module Main exposing (..)

import Browser
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
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
    , reportPage : ReportPage
    }


type ReportPage
    = RawReport
    | GroupedByPayer
    | GroupedByDate


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
      , reportPage = RawReport
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
    | ReportPageButtonClicked ReportPage


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

        ReportPageButtonClicked reportPage ->
            ( { model | reportPage = reportPage }
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
                viewUploadStatement model.statementHasHeaders

            Loading ->
                Html.div [] [ Html.text "loading" ]

            DefiningColumns ->
                viewDefineColumns model.allStatementColumns model.statementHeaders model.statementHasHeaders

            Ready report ->
                Html.div [ Attrs.class "flex flex-col justify-center items-center w-full" ]
                    [ Html.div [ Attrs.class "flex flex-row gap-4" ]
                        [ Html.input
                            [ Attrs.type_ "button"
                            , Attrs.value "raw report"
                            , Events.onClick (ReportPageButtonClicked RawReport)
                            , reportaPageButtonStyle (model.reportPage == RawReport)
                            ]
                            []
                        , Html.input
                            [ Attrs.type_ "button"
                            , Attrs.value "Group By Payer"
                            , Events.onClick (ReportPageButtonClicked GroupedByPayer)
                            , reportaPageButtonStyle (model.reportPage == GroupedByPayer)
                            ]
                            []
                        , Html.input
                            [ Attrs.type_ "button"
                            , Attrs.value "Group By Date"
                            , Events.onClick (ReportPageButtonClicked GroupedByDate)
                            , reportaPageButtonStyle (model.reportPage == GroupedByDate)
                            ]
                            []
                        ]
                    , case model.reportPage of
                        RawReport ->
                            viewRawReport report

                        GroupedByPayer ->
                            Html.span [] []

                        GroupedByDate ->
                            Html.span [] []
                    ]
        ]
    }


reportaPageButtonStyle isActive =
    Attrs.class
        (([ "p-2 mb-2 border border-stone-500 rounded" ]
            ++ (if isActive then
                    [ "bg-green-400" ]

                else
                    []
               )
         )
            |> String.join " "
        )


viewRawReport : Dict Int StatementRow -> Html msg
viewRawReport report =
    Html.div [ Attrs.class "flex flex-col w-full" ]
        (Dict.toList report
            |> List.sortBy (Tuple.second >> .dateOfPayment >> Time.posixToMillis)
            |> List.map
                (\( _, reportRow ) ->
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


viewUploadStatement : Bool -> Html Msg
viewUploadStatement hasHeaders =
    Html.div [ Attrs.class "flex justify-center items-center h-[100vh] w-full" ]
        [ Html.div [ Attrs.class "flex flex-col justify-center border rounded p-8" ]
            [ Html.input
                [ Attrs.type_ "button"
                , Attrs.value "upload file"
                , Attrs.class "p-2 m-2 border rounded"
                , Events.onClick FileRequested
                ]
                []
            , Html.div
                [ Attrs.class "flex flex-row items-center border rounded bg-stone-100 p-1 hover:bg-cyan-100"
                , Events.onClick (FileHasHeadersChecked (not hasHeaders))
                ]
                [ Html.input
                    [ Attrs.type_ "checkbox"
                    , Attrs.class "p-2 m-2 border rounded"
                    , Attrs.checked hasHeaders
                    , Events.onCheck FileHasHeadersChecked
                    ]
                    []
                , Html.label [] [ Html.text "report contains headers" ]
                ]
            ]
        ]


viewDefineColumns : List ( Int, ColumnType, List String ) -> Dict Int String -> Bool -> Html Msg
viewDefineColumns allStatementColumns statementHeaders hasHeaders =
    Html.div [ Attrs.class "flex flex-col gap-2 justify-center items-center w-full" ]
        [ Html.div [ Attrs.class "flex justify-center items-center w-full sticky top-0 py-2 mb-2 bg-stone-500" ]
            [ Html.input
                [ Attrs.type_ "button"
                , Attrs.value "submit"
                , Attrs.class "p-4 bg-green-400 text-white border-2 border-stone-500 rounded"
                , Events.onClick ColumnsSubmitted
                ]
                []
            ]
        , Html.div [ Attrs.class "w-3/4 flex gap-8 flex-wrap justify-center" ]
            (allStatementColumns
                |> List.map
                    (\( index, columnType, v ) ->
                        Html.div [ Attrs.class "flex flex-col w-96 gap-2 border rounded p-4" ]
                            (Html.div [ Attrs.class "flex flex-row gap-4 justify-around" ]
                                [ viewColumnTypeSelect hasHeaders
                                    columnType
                                    index
                                    (Dict.get index statementHeaders
                                        |> Maybe.withDefault ""
                                    )
                                ]
                                :: (List.take 4 v
                                        |> List.map
                                            (\cell ->
                                                Html.p [ Attrs.class "truncate" ]
                                                    [ Html.text cell ]
                                            )
                                   )
                            )
                    )
            )
        ]


viewColumnTypeSelect : Bool -> ColumnType -> Int -> String -> Html Msg
viewColumnTypeSelect hasHeaders columnType index header =
    Html.div [ Attrs.class "flex flex-row w-full justify-between items-center border-b my-1 pb-2" ]
        (case columnType of
            CommonInfo info ->
                [ if hasHeaders then
                    Html.label [ Attrs.class "w-2/3" ] [ Html.text header ]

                  else
                    Html.input
                        [ Attrs.type_ "text"
                        , Attrs.value info
                        , Events.onInput (CommonInfoHeaderChanged index)
                        ]
                        []
                , viewTypeSelect index header
                ]

            Payer ->
                [ Html.label [ Attrs.class "w-2/3" ] [ Html.text header ]
                , viewTypeSelect index header
                ]

            Date ->
                [ Html.label [ Attrs.class "w-2/3" ] [ Html.text header ]
                , viewTypeSelect index header
                ]

            Value ->
                [ Html.label [ Attrs.class "w-2/3" ] [ Html.text header ]
                , viewTypeSelect index header
                ]
        )


viewTypeSelect : Int -> String -> Html Msg
viewTypeSelect index header =
    Html.select
        [ Events.onInput (ColumnTypeSelected index)
        , Attrs.class "w-1/3 bg-stone-100 p-2 border rounded"
        ]
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
