module ReportChart exposing (ChartItem, toYearlyReports, viewYearReportChart)

import Chart
import Chart.Attributes as ChartAttrs
import Chart.Item as ChartItem
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attrs
import Report
import Time


type alias ChartItem =
    ChartItem.One
        { value : Float
        , index : Float
        , date : Time.Posix
        }
        ChartItem.Dot


viewYearReportChart : ( Int, Dict Int Float ) -> Html msg
viewYearReportChart ( year, monthlyReport ) =
    Html.div [ Attrs.class "pb-4" ]
        [ Html.p [ Attrs.class "py-4" ]
            [ Html.text (String.fromInt year ++ " year") ]
        , Chart.chart
            [ ChartAttrs.height 80
            , ChartAttrs.htmlAttrs
                [ Attrs.class "w-full px-4 pb-12 pt-24 border rounded" ]
            ]
            [ Chart.binLabels
                (\( month, _ ) -> String.fromInt month)
                [ ChartAttrs.moveDown 6, ChartAttrs.fontSize 5 ]
            , Chart.yLabels
                [ ChartAttrs.withGrid
                , ChartAttrs.fontSize 0
                ]
            , Chart.bars
                []
                [ Chart.bar
                    (\( _, value ) ->
                        ((value * 100)
                            |> Basics.round
                            |> Basics.toFloat
                        )
                            / 100
                    )
                    [ ChartAttrs.color "lightblue" ]
                ]
                (monthlyReport |> Dict.toList)
            , Chart.barLabels
                [ ChartAttrs.fontSize 5
                , ChartAttrs.moveUp 2
                , ChartAttrs.color "stone"
                ]
            ]
        ]


toYearlyReports : List Report.Row -> Dict Int (Dict Int Float)
toYearlyReports rows =
    rows
        |> List.map
            (\row ->
                let
                    year : Int
                    year =
                        Time.toYear Time.utc row.dateOfPayment
                in
                ( year, row )
            )
        |> List.foldl
            (\( year, row ) acc ->
                case Dict.get year acc of
                    Just value ->
                        Dict.insert year (row :: value) acc

                    Nothing ->
                        Dict.insert year [ row ] acc
            )
            Dict.empty
        |> Dict.map
            (\_ rows_ ->
                rows_
                    |> List.map
                        (\row ->
                            ( monthToInt (Time.toMonth Time.utc row.dateOfPayment)
                            , row.valueOfPayment
                            )
                        )
                    |> List.append allMonthsNumbersWithZeroValues
                    |> List.foldl
                        (\( month, valueOfPayment ) acc ->
                            case Dict.get month acc of
                                Just monthTotal ->
                                    Dict.insert month (valueOfPayment + monthTotal) acc

                                Nothing ->
                                    Dict.insert month valueOfPayment acc
                        )
                        Dict.empty
            )


allMonthsNumbersWithZeroValues : List ( Int, Float )
allMonthsNumbersWithZeroValues =
    [ ( 1, 0 )
    , ( 2, 0 )
    , ( 3, 0 )
    , ( 4, 0 )
    , ( 5, 0 )
    , ( 6, 0 )
    , ( 7, 0 )
    , ( 8, 0 )
    , ( 9, 0 )
    , ( 10, 0 )
    , ( 11, 0 )
    , ( 12, 0 )
    ]


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12
