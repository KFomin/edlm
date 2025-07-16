module Main exposing (..)

import Browser
import Dict
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Report
import ReportChart
import Task


main : Program {} Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { report : Report.Report
    , reportColumns : List ( Int, Report.ColumnType, List String )
    , reportHasHeaders : Bool
    , expanded : Maybe String
    }


init : {} -> ( Model, Cmd msg )
init _ =
    ( { report = Report.NotAsked
      , reportHasHeaders = False
      , reportColumns = []
      , expanded = Nothing
      }
    , Cmd.none
    )


type Msg
    = FileHasHeadersChecked Bool
    | FileRequested
    | FileSelected File
    | FileLoaded String
    | ColumnsSubmitted
    | ColumnTypeSelected Int String
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
            ( { model | report = Report.Loading }
            , Task.perform FileLoaded (File.toString file)
            )

        FileLoaded fileContent ->
            ( { model
                | report = Report.DefiningColumns
                , reportColumns = Report.parse fileContent model.reportHasHeaders
              }
            , Cmd.none
            )

        ColumnsSubmitted ->
            ( { model
                | report =
                    model.reportColumns
                        |> Report.fromColumns
                        |> Report.Ready
              }
            , Cmd.none
            )

        ColumnTypeSelected selectedColumnIndex selectedColumnType ->
            ( { model
                | reportColumns =
                    List.map
                        (\( columnIndex, columnType, cells ) ->
                            if columnIndex == selectedColumnIndex then
                                ( columnIndex
                                , Report.columnTypeFromString selectedColumnType
                                , cells
                                )

                            else
                                ( columnIndex
                                , columnType
                                , cells
                                )
                        )
                        model.reportColumns
              }
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


view : Model -> Browser.Document Msg
view ({ report } as model) =
    { title = "everyday life management"
    , body =
        [ case report of
            Report.NotAsked ->
                viewUploadStatement model.reportHasHeaders

            Report.Loading ->
                Html.div [] [ Html.text "loading" ]

            Report.DefiningColumns ->
                viewDefineColumns model.reportColumns

            Report.Ready readyReport ->
                Html.div [ Attrs.class "flex flex-col justify-center items-center w-full" ]
                    [ viewGroupedByPayer
                        { rows = Dict.values readyReport
                        , expanded = model.expanded
                        }
                    ]
        ]
    }


viewGroupedByPayer : { rows : List Report.Row, expanded : Maybe String } -> Html Msg
viewGroupedByPayer { rows, expanded } =
    let
        groupsByPayer : List ( { payer : String, totalSum : Float }, List Report.Row )
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


viewDefineColumns : List ( Int, Report.ColumnType, List String ) -> Html Msg
viewDefineColumns allStatementColumns =
    Html.div [ Attrs.class "flex flex-col gap-2 m-2 justify-center items-center w-full" ]
        [ Html.div [ Attrs.class "w-3/4 flex gap-8 flex-wrap justify-center" ]
            (allStatementColumns
                |> List.map
                    (\( index, _, v ) ->
                        Html.div [ Attrs.class "flex flex-col w-82 gap-2 border rounded p-4" ]
                            (Html.div [ Attrs.class "flex flex-row gap-4 justify-around" ]
                                [ Report.viewColumnTypeSelect index ColumnTypeSelected ]
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
        , Html.div
            [ Attrs.class "flex justify-center items-center w-full py-2 mb-2" ]
            [ Html.input
                [ Attrs.type_ "button"
                , Attrs.value "confirm"
                , Attrs.class "p-4 bg-green-400 text-white border-2 border-stone-500 rounded"
                , Events.onClick ColumnsSubmitted
                ]
                []
            ]
        ]
