module Report exposing
    ( ColumnType(..)
    , Model
    , Msg
    , Report(..)
    , Row
    , columnTypeFromString
    , fromColumns
    , parse
    , update
    , view
    , viewColumnTypeSelect
    )

import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Iso8601
import Json.Decode as Decode
import ReportChart
import Task
import Time


type alias Model =
    { report : Report
    , requiredColumnsDefined : Bool
    , reportColumns :
        List
            ( Int
            , Maybe ColumnType
            , List String
            )
    , reportHasHeaders : Bool
    , expanded : Maybe String
    }


type Report
    = NotAsked
    | Loading
    | DefiningColumns
    | Ready (Dict Int Row)


type ColumnType
    = PaymentRecipient
    | PaymentDate
    | PaymentAmount


type alias Row =
    { payerOrReceiver : String
    , dateOfPayment : Time.Posix
    , valueOfPayment : Float
    , allOtherInfo : List String
    }


columnTypeToString : ColumnType -> String
columnTypeToString columnType =
    case columnType of
        PaymentRecipient ->
            "payment recipient"

        PaymentDate ->
            "date of payment"

        PaymentAmount ->
            "payment amount"


columnTypeFromString : String -> Maybe ColumnType
columnTypeFromString columnType =
    case columnType of
        "payment recipient" ->
            Just PaymentRecipient

        "date of payment" ->
            Just PaymentDate

        "payment amount" ->
            Just PaymentAmount

        _ ->
            Nothing


type Msg
    = FileHasHeadersChecked Bool
    | FileRequested
    | FileSelected File
    | FileLoaded String
    | ColumnsSubmitted
    | ColumnTypeSelected Int (Maybe ColumnType)
    | GroupExpanded String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileHasHeadersChecked fileHasHeader ->
            ( { model | reportHasHeaders = fileHasHeader }
            , Cmd.none
            )

        FileRequested ->
            ( model
            , Select.file [ "text/csv" ] FileSelected
            )

        FileSelected file ->
            ( { model | report = Loading }
            , Task.perform FileLoaded (File.toString file)
            )

        FileLoaded fileContent ->
            ( { model
                | report = DefiningColumns
                , reportColumns = parse fileContent model.reportHasHeaders
              }
            , Cmd.none
            )

        ColumnsSubmitted ->
            ( { model
                | report =
                    model.reportColumns
                        |> fromColumns
                        |> Ready
              }
            , Cmd.none
            )

        ColumnTypeSelected selectedColumnIndex selectedColumnType ->
            ( { model
                | reportColumns =
                    model.reportColumns
                        |> List.map
                            (\( columnIndex, columnType, cells ) ->
                                if columnIndex == selectedColumnIndex then
                                    ( columnIndex
                                    , selectedColumnType
                                    , cells
                                    )

                                else if
                                    (columnType == selectedColumnType)
                                        && (columnIndex /= selectedColumnIndex)
                                then
                                    ( columnIndex
                                    , Nothing
                                    , cells
                                    )

                                else
                                    ( columnIndex
                                    , columnType
                                    , cells
                                    )
                            )
              }
            , Cmd.none
            )
                |> Tuple.mapFirst
                    (\model_ ->
                        { model_
                            | requiredColumnsDefined =
                                List.map (\( _, columnType, _ ) -> columnType) model_.reportColumns
                                    |> List.filterMap identity
                                    |> List.length
                                    |> (==) 3
                        }
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


parse : String -> Bool -> List ( Int, Maybe ColumnType, List String )
parse stringReport fileHasHeaders =
    let
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

        rowsOfNormalizedCells : List (List ( Int, String ))
        rowsOfNormalizedCells =
            rawRows
                |> List.map (String.split ";")
                |> List.map
                    (List.indexedMap
                        (\index cell ->
                            ( index, normalizeStringCell cell )
                        )
                    )
    in
    rowsOfNormalizedCells
        |> List.foldr
            (\row acc ->
                row
                    |> List.foldr
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
            )
            Dict.empty
        |> Dict.toList
        |> List.map (\( index, v ) -> ( index, Nothing, v ))
        |> List.filter (\( _, _, cells ) -> List.all (String.isEmpty >> not) cells)


toData :
    List ( Int, Maybe ColumnType, List String )
    ->
        { payers : Dict Int String
        , values : Dict Int Float
        , dates : Dict Int Time.Posix
        , commonInfo : Dict Int (List String)
        }
toData =
    List.foldr
        (\( _, columnType, cells ) acc ->
            cells
                |> List.indexedMap
                    (\index cell ->
                        ( index, cell )
                    )
                |> List.foldr
                    (\( index, cell ) data ->
                        case columnType of
                            Just PaymentRecipient ->
                                { data
                                    | payers =
                                        if String.isEmpty cell then
                                            data.payers

                                        else
                                            Dict.insert index
                                                cell
                                                data.payers
                                }

                            Just PaymentDate ->
                                { data
                                    | dates =
                                        (Iso8601.toTime
                                            (String.split "." cell
                                                |> List.reverse
                                                |> String.join "-"
                                            )
                                            |> Result.toMaybe
                                        )
                                            |> Maybe.map
                                                (\dates ->
                                                    Dict.insert index
                                                        dates
                                                        data.dates
                                                )
                                            |> Maybe.withDefault data.dates
                                }

                            Just PaymentAmount ->
                                { data
                                    | values =
                                        String.replace "," "." cell
                                            |> String.toFloat
                                            |> Maybe.map
                                                (\flt ->
                                                    Dict.insert index
                                                        flt
                                                        data.values
                                                )
                                            |> Maybe.withDefault data.values
                                }

                            Nothing ->
                                data
                    )
                    acc
        )
        { payers = Dict.empty
        , values = Dict.empty
        , dates = Dict.empty
        , commonInfo = Dict.empty
        }


fromColumns :
    List ( Int, Maybe ColumnType, List String )
    -> Dict Int Row
fromColumns columns =
    toData columns
        |> (\{ payers, dates, values, commonInfo } ->
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
           )


view : Model -> Html Msg
view model =
    case model.report of
        NotAsked ->
            viewUploadStatement model.reportHasHeaders

        Loading ->
            Html.div [] [ Html.text "loading" ]

        DefiningColumns ->
            Html.div [ Attrs.class "w-full" ]
                [ viewDefineColumns model.reportColumns
                , Html.div
                    [ Attrs.class "flex justify-center items-center w-full py-2 mb-2" ]
                    [ Html.input
                        [ Attrs.type_ "button"
                        , Attrs.value "confirm"
                        , Attrs.disabled (not model.requiredColumnsDefined)
                        , Attrs.class "p-4 bg-green-400 text-white border-2 border-stone-500 rounded"
                        , Events.onClick ColumnsSubmitted
                        ]
                        []
                    ]
                ]

        Ready readyReport ->
            Html.div
                [ Attrs.class "flex flex-col justify-center items-center w-full" ]
                [ viewGroupedByPayer
                    { rows = Dict.values readyReport
                    , expanded = model.expanded
                    }
                ]


viewGroupedByPayer : { rows : List Row, expanded : Maybe String } -> Html Msg
viewGroupedByPayer { rows, expanded } =
    let
        groupsByPayer : List ( { payer : String, totalSum : Float }, List Row )
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
    Html.div [ Attrs.class "flex flex-col justify-between lg:w-3/4 md:w-full md:px-1" ]
        (groupsByPayer
            |> List.map
                (\( { payer, totalSum }, groupRows ) ->
                    Html.div
                        [ Attrs.class "flex flex-col justify-center w-full"
                        ]
                        (Html.div
                            [ Attrs.class "flex flex-row justify-between p-2 my-1"
                            , Attrs.class "border-b rounded-t bg-stone-200"
                            , Events.onClick (GroupExpanded payer)
                            ]
                            [ Html.p [] [ Html.text payer ]
                            , Html.span [ Attrs.class "flex flex-row gap-4" ]
                                [ Html.p [] [ Html.text (String.fromFloat totalSum) ] ]
                            ]
                            :: (if Just payer == expanded then
                                    ReportChart.toYearlyReports groupRows
                                        |> Dict.toList
                                        |> List.map ReportChart.viewYearReportChart

                                else
                                    []
                               )
                        )
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
                , Html.label [] [ Html.text "statement contains headers" ]
                ]
            ]
        ]


viewDefineColumns : List ( Int, Maybe ColumnType, List String ) -> Html Msg
viewDefineColumns allStatementColumns =
    Html.div [ Attrs.class "flex flex-col gap-2 m-2 justify-center items-center w-full" ]
        [ Html.div [ Attrs.class "w-3/4 flex gap-8 flex-wrap justify-center" ]
            (allStatementColumns
                |> List.map
                    (\( index, columnTypeSelected, v ) ->
                        Html.div
                            [ Attrs.class
                                ([ "flex flex-col w-82 gap-2 border rounded p-4"
                                 , columnTypeSelected
                                    |> Maybe.map (\_ -> "bg-green-100")
                                    |> Maybe.withDefault ""
                                 ]
                                    |> String.join " "
                                )
                            ]
                            (Html.div [ Attrs.class "flex flex-row gap-4 justify-around" ]
                                [ viewColumnTypeSelect index columnTypeSelected ColumnTypeSelected ]
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


viewColumnTypeSelect : Int -> Maybe ColumnType -> (Int -> Maybe ColumnType -> msg) -> Html msg
viewColumnTypeSelect index columnTypeSelected toSelectedMsg =
    Html.div
        [ Attrs.class "flex flex-col w-full justify-between items-center border-b my-1 pb-2" ]
        [ viewTypeSelect index columnTypeSelected toSelectedMsg ]


viewTypeSelect : Int -> Maybe ColumnType -> (Int -> Maybe ColumnType -> msg) -> Html msg
viewTypeSelect index columnTypeSelected toSelectedMsg =
    Html.select
        [ Events.onInput
            (\columnTypeString ->
                toSelectedMsg index
                    (columnTypeFromString columnTypeString)
            )
        , Attrs.class
            (columnTypeSelected
                |> Maybe.map (\_ -> "")
                |> Maybe.withDefault "text-stone-500"
            )
        , Attrs.class "w-2/3 bg-stone-50 p-2 border rounded items-center text-center"
        ]
        [ Html.option [ Attrs.selected (columnTypeSelected == Nothing) ]
            [ Html.text "select column type" ]
        , Html.option
            [ Attrs.value (columnTypeToString PaymentRecipient), Attrs.selected (columnTypeSelected == Just PaymentRecipient) ]
            [ Html.text (columnTypeToString PaymentRecipient) ]
        , Html.option
            [ Attrs.value (columnTypeToString PaymentDate), Attrs.selected (columnTypeSelected == Just PaymentDate) ]
            [ Html.text (columnTypeToString PaymentDate) ]
        , Html.option
            [ Attrs.value (columnTypeToString PaymentAmount), Attrs.selected (columnTypeSelected == Just PaymentAmount) ]
            [ Html.text (columnTypeToString PaymentAmount) ]
        ]
