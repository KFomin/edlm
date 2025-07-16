module Report exposing
    ( ColumnType(..)
    , Report(..)
    , Row
    , columnTypeFromString
    , fromColumns
    , parse
    , viewColumnTypeSelect
    )

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Iso8601
import Json.Decode as Decode
import Time


type Report
    = NotAsked
    | Loading
    | DefiningColumns
    | Ready (Dict Int Row)


type ColumnType
    = CommonInfo
    | Payer
    | Date
    | Value


type alias Row =
    { payerOrReceiver : String
    , dateOfPayment : Time.Posix
    , valueOfPayment : Float
    , allOtherInfo : List String
    }


translateColumnType : ColumnType -> String
translateColumnType columnType =
    case columnType of
        CommonInfo ->
            "common info"

        Payer ->
            "payment receiver"

        Date ->
            "date of payment"

        Value ->
            "money paid"


columnTypeToString : ColumnType -> String
columnTypeToString columnType =
    case columnType of
        CommonInfo ->
            "commoninfo"

        Payer ->
            "payment receiver"

        Date ->
            "date of payment"

        Value ->
            "money paid"


columnTypeFromString : String -> ColumnType
columnTypeFromString columnType =
    case columnType of
        "commoninfo" ->
            CommonInfo

        "payment receiver" ->
            Payer

        "date of payment" ->
            Date

        "money paid" ->
            Value

        _ ->
            CommonInfo


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


parse : String -> Bool -> List ( Int, ColumnType, List String )
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
        |> List.map (\( index, v ) -> ( index, CommonInfo, v ))
        |> List.filter (\( _, _, cells ) -> List.all (String.isEmpty >> not) cells)


toData :
    List ( Int, ColumnType, List String )
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
                            CommonInfo ->
                                { data
                                    | commonInfo =
                                        Dict.insert index
                                            (case Dict.get index data.commonInfo of
                                                Just commonInfos ->
                                                    cell :: commonInfos

                                                Nothing ->
                                                    [ cell ]
                                            )
                                            data.commonInfo
                                }

                            Payer ->
                                { data
                                    | payers =
                                        if String.isEmpty cell then
                                            data.payers

                                        else
                                            Dict.insert index
                                                cell
                                                data.payers
                                }

                            Date ->
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
                                                (\bitcherzz ->
                                                    Dict.insert index
                                                        bitcherzz
                                                        data.dates
                                                )
                                            |> Maybe.withDefault data.dates
                                }

                            Value ->
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
                    )
                    acc
        )
        { payers = Dict.empty
        , values = Dict.empty
        , dates = Dict.empty
        , commonInfo = Dict.empty
        }


fromColumns :
    List ( Int, ColumnType, List String )
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


viewColumnTypeSelect : Int -> (Int -> String -> msg) -> Html msg
viewColumnTypeSelect index toSelectedMsg =
    Html.div
        [ Attrs.class "flex flex-col w-full justify-between items-center border-b my-1 pb-2" ]
        [ viewTypeSelect index toSelectedMsg ]


viewTypeSelect : Int -> (Int -> String -> msg) -> Html msg
viewTypeSelect index toSelectedMsg =
    Html.select
        [ Events.onInput (toSelectedMsg index)
        , Attrs.class "w-2/3 bg-stone-100 p-2 border rounded items-center text-center"
        ]
        [ Html.option
            [ Attrs.value (columnTypeToString CommonInfo) ]
            [ Html.text (translateColumnType CommonInfo) ]
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
