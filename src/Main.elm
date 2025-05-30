port module Main exposing (..)

import Browser
import Chart
import Chart.Attributes as ChartAttrs
import Chart.Events as ChartEvents
import Chart.Item as ChartItem
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


port toggleGroupDialog : String -> Cmd msg


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
    , expanded : Maybe String
    , groupDialogData : Maybe GroupDialogData
    , hovering : List (ChartItem.One { value : Float, index : Float, date : Time.Posix } ChartItem.Dot)
    }


type alias GroupDialogData =
    { payer : String
    , rows : List StatementRow
    }


type ReportPage
    = RawReport
    | GroupedByPayer


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
      , expanded = Nothing
      , groupDialogData = Nothing
      , hovering = []
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
    | GroupExpanded String
    | GroupChartClicked String (List StatementRow)
    | GroupDialogToggled
    | OnChartPointHover (List (ChartItem.One { value : Float, index : Float, date : Time.Posix } ChartItem.Dot))


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

        GroupExpanded string ->
            ( { model
                | expanded =
                    if Just string == model.expanded then
                        Nothing

                    else
                        Just string
              }
            , Cmd.none
            )

        GroupChartClicked payer statementRows ->
            ( { model
                | groupDialogData =
                    Just
                        { payer = payer
                        , rows = statementRows
                        }
              }
            , toggleGroupDialog "report-group-dialog"
            )

        GroupDialogToggled ->
            ( model
            , toggleGroupDialog "report-group-dialog"
            )

        OnChartPointHover chartItems ->
            ( { model | hovering = chartItems }
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
                                            (String.split "." cell
                                                |> List.reverse
                                                |> String.join "-"
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
                                        String.replace "," "." cell
                                            |> String.toFloat
                                            |> Maybe.map
                                                (\flt ->
                                                    Dict.insert index
                                                        flt
                                                        stuffs.values
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
        normalizeStringCell : String -> String
        normalizeStringCell cell =
            case Decode.decodeString Decode.string cell of
                Ok normalizedCell ->
                    if String.isEmpty normalizedCell then
                        cell

                    else
                        normalizedCell

                Err _ ->
                    cell

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
                                |> List.indexedMap (\index header -> ( index, normalizeStringCell header ))
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
                            ( index, normalizeStringCell cell )
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
                viewDefineColumns model.allStatementColumns
                    model.statementHeaders
                    model.statementHasHeaders

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
                        ]
                    , case model.reportPage of
                        RawReport ->
                            viewRawReport report

                        GroupedByPayer ->
                            viewGroupedByPayer
                                { rows = Dict.values report
                                , expanded = model.expanded
                                }
                                model.hovering
                    ]
        , viewGroupDialog model.groupDialogData
        ]
    }


viewGroupDialog : Maybe GroupDialogData -> Html Msg
viewGroupDialog maybeDialogData =
    Html.node "dialog"
        [ Attrs.id "report-group-dialog"
        , Attrs.class "backdrop:bg-black/50 backdrop:backdrop-blur-md backdrop:fixed max-h-screen"
        , Attrs.class "fixed left-[50%] top-[50%] translate-x-[-50%] translate-y-[-50%]"
        , Attrs.class "min-w-[500px] w-full h-full"
        ]
        (case maybeDialogData of
            Just dialogData ->
                let
                    chartData : List { valueOfPayment : Float, dateOfPayment : Float }
                    chartData =
                        dialogData.rows
                            |> List.sortBy (.dateOfPayment >> Time.posixToMillis)
                            |> List.map (\{ valueOfPayment, dateOfPayment } -> { valueOfPayment = valueOfPayment, dateOfPayment = Time.posixToMillis dateOfPayment |> Basics.toFloat })
                in
                [ Html.div
                    [ Attrs.class "flex flex-col items-end gap-2 p-2"
                    ]
                    [ Html.input
                        [ Attrs.type_ "button"
                        , Attrs.value "close"
                        , reportaPageButtonStyle True
                        , Events.onClick GroupDialogToggled
                        ]
                        []
                    , Chart.chart []
                        [ Chart.xLabels []
                        , Chart.yLabels [ ChartAttrs.withGrid ]
                        , Chart.series .valueOfPayment
                            [ Chart.interpolated
                                .dateOfPayment
                                []
                                []
                            ]
                            chartData
                        ]
                    ]
                ]

            Nothing ->
                []
        )


viewGroupedByPayer :
    { rows : List StatementRow, expanded : Maybe String }
    ->
        List
            (ChartItem.One
                { value : Float
                , index : Float
                , date : Time.Posix
                }
                ChartItem.Dot
            )
    -> Html Msg
viewGroupedByPayer { rows, expanded } hovering =
    let
        groupsByPayer : List ( { payer : String, totalSum : Float }, List StatementRow )
        groupsByPayer =
            rows
                |> List.map (\row -> ( row.payerOrReceiver, row ))
                |> List.foldl
                    (\( payer, row ) acc ->
                        if payer == "\"\"" then
                            acc

                        else
                            case Dict.get payer acc of
                                Just group ->
                                    Dict.insert payer (row :: group) acc

                                Nothing ->
                                    Dict.insert payer [ row ] acc
                    )
                    Dict.empty
                |> Dict.toList
                |> List.map
                    (\( payer, groupRows ) ->
                        ( { payer = payer
                          , totalSum =
                                groupRows
                                    |> List.map .valueOfPayment
                                    |> List.sum
                          }
                        , groupRows
                        )
                    )
                |> List.sortBy (Tuple.first >> .totalSum)
    in
    Html.div [ Attrs.class "flex flex-col justify-between w-3/4" ]
        (groupsByPayer
            |> List.map
                (\( { payer, totalSum }, groupRows ) ->
                    let
                        chartData : List { value : Float, index : Float, date : String }
                        chartData =
                            groupRows
                                |> List.sortBy (.dateOfPayment >> Time.posixToMillis)
                                |> List.indexedMap
                                    (\index { valueOfPayment, dateOfPayment } ->
                                        { value = valueOfPayment
                                        , index = Basics.toFloat index
                                        , date = Iso8601.fromTime dateOfPayment
                                        }
                                    )
                                |> List.foldl
                                    (\{ value, index, date } acc ->
                                        case Dict.get date acc of
                                            Nothing ->
                                                Dict.insert date { value = value, index = index } acc

                                            Just dictValue ->
                                                Dict.insert date { value = value + dictValue.value, index = dictValue.index } acc
                                    )
                                    Dict.empty
                                |> Dict.toList
                                |> List.map
                                    (\( date, { value, index } ) ->
                                        { value = value
                                        , index = index
                                        , date = date
                                        }
                                    )
                    in
                    Html.div
                        [ Attrs.class "flex flex-col w-full"
                        ]
                        ([ Html.div
                            [ Attrs.class "flex flex-row justify-between border-b rounded-t p-2 my-1 bg-stone-200"
                            , Events.onClick (GroupExpanded payer)
                            ]
                            [ Html.p [] [ Html.text payer ]
                            , Html.span [ Attrs.class "flex flex-row gap-4" ]
                                [ Html.p [] [ Html.text (String.fromFloat totalSum) ]
                                ]
                            ]
                         ]
                            ++ (if Just payer == expanded then
                                    [ [ Chart.chart
                                            [ ChartAttrs.height 100
                                            , ChartAttrs.htmlAttrs [ Attrs.class "w-full px-4 pb-4 pt-24 border rounded" ]
                                            ]
                                            [ --, Chart.series .index
                                              --    [ Chart.interpolated .value
                                              --        []
                                              --        [ ChartAttrs.circle, ChartAttrs.border "1px solid black" ]
                                              --    ]
                                              --    (chartData
                                              --        |> List.map
                                              --            (\{ value, index, date } ->
                                              --                { index = index
                                              --                , value = value
                                              --                , date = date
                                              --                }
                                              --            )
                                              --    )
                                              Chart.bars
                                                []
                                                [ Chart.bar .value [ ChartAttrs.color "lightblue" ] ]
                                                chartData
                                            , Chart.barLabels
                                                [ ChartAttrs.fontSize 4
                                                , ChartAttrs.rotate 45
                                                , ChartAttrs.color "stone"
                                                , ChartAttrs.alignLeft
                                                ]
                                            ]
                                      ]
                                    , groupRows |> List.map viewRawReportRow
                                    ]
                                        |> List.concat

                                else
                                    []
                               )
                        )
                )
        )


reportaPageButtonStyle : Bool -> Html.Attribute msg
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
            |> List.map (Tuple.second >> viewRawReportRow)
        )


viewRawReportRow : StatementRow -> Html msg
viewRawReportRow reportRow =
    Html.div [ Attrs.class "flex flex-col justify-center items-center" ]
        [ Html.div [ Attrs.class "flex flex-row gap-4 w-full border-b" ]
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
