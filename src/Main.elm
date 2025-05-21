port module Main exposing (..)

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


port toggleGroupDialog : String -> Cmd msg


type alias Model =
    { statement : Statement ReportRow
    , undefinedColumns : List ( Int, ColumnType, List String )
    , fileHasHeaders : Bool
    , headers : Dict Int String
    }


type alias ReportRow =
    { payer : String
    , date : Time.Posix
    , value : Float
    , commonInfo : List ( String, String )
    }


init : {} -> ( Model, Cmd msg )
init _ =
    ( { statement = NotAsked
      , fileHasHeaders = False
      , undefinedColumns = []
      , headers = Dict.empty
      }
    , Cmd.none
    )


type Statement entity
    = NotAsked
    | Loading
    | DefiningColumns
    | Success (Dict Int entity)


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
            ( { model | fileHasHeaders = fileHasHeader }
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
                    toColumns fileContent model.fileHasHeaders
            in
            ( { model
                | statement = DefiningColumns
                , undefinedColumns = columns
                , headers = headers
              }
            , Cmd.none
            )

        ColumnsSubmitted ->
            let
                dataStuff :
                    { payers : Dict Int String
                    , values : Dict Int Float
                    , dates : Dict Int Time.Posix
                    , commonInfos : Dict Int (List ( String, String ))
                    }
                dataStuff =
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
                                                    | commonInfos =
                                                        Dict.insert index
                                                            (case Dict.get index stuffs.commonInfos of
                                                                Just commonInfos ->
                                                                    ( info, cell ) :: commonInfos

                                                                Nothing ->
                                                                    [ ( info, cell ) ]
                                                            )
                                                            stuffs.commonInfos
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
                                                                    let
                                                                        _ =
                                                                            Debug.log "cleanCell" cleanCell
                                                                    in
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
                        , commonInfos = Dict.empty
                        }
                        model.undefinedColumns

                report : Dict Int ReportRow
                report =
                    dataStuff.payers
                        |> Dict.foldr
                            (\k payer acc ->
                                let
                                    date =
                                        Dict.get k dataStuff.dates

                                    value =
                                        Dict.get k dataStuff.values

                                    commonInfo =
                                        Dict.get k dataStuff.commonInfos
                                in
                                Maybe.map2
                                    (\d v ->
                                        Dict.insert k
                                            { payer = payer
                                            , date = d
                                            , value = v
                                            , commonInfo = commonInfo |> Maybe.withDefault []
                                            }
                                            acc
                                    )
                                    date
                                    value
                                    |> Maybe.withDefault acc
                            )
                            Dict.empty

                _ =
                    Debug.log "AAAAA" report
            in
            ( model
            , Cmd.none
            )

        ColumnTypeSelected index columnType ->
            ( { model
                | undefinedColumns =
                    List.map
                        (\( i, ct, cells ) ->
                            if i == index then
                                ( i, columnTypeFromString columnType, cells )

                            else
                                ( i, ct, cells )
                        )
                        model.undefinedColumns
              }
            , Cmd.none
            )

        CommonInfoHeaderChanged index commonInfo ->
            ( { model
                | undefinedColumns =
                    List.map
                        (\( i, ct, cells ) ->
                            if i == index then
                                ( i, CommonInfo commonInfo, cells )

                            else
                                ( i, ct, cells )
                        )
                        model.undefinedColumns
              }
            , Cmd.none
            )


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
                    ((model.undefinedColumns
                        |> List.map
                            (\( index, columnType, v ) ->
                                Html.div [ Attrs.class "flex flex-col w-64 gap-2 border rounded p-4" ]
                                    (Html.div [ Attrs.class "flex flex-row gap-4 justify-around" ]
                                        [ viewColumnTypeSelect columnType index (Dict.get index model.headers |> Maybe.withDefault "") ]
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

            Success a ->
                Html.div []
                    [ Html.text "Success" ]
        ]
    }


viewColumnTypeSelect : ColumnType -> Int -> String -> Html.Html Msg
viewColumnTypeSelect columnType index header =
    Html.div []
        [ case columnType of
            CommonInfo info ->
                Html.input [ Attrs.type_ "text", Attrs.value info, Events.onInput (CommonInfoHeaderChanged index) ]
                    []

            Payer ->
                Html.span [] []

            Date ->
                Html.span [] []

            Value ->
                Html.span [] []
        , Html.select [ Events.onInput (ColumnTypeSelected index) ]
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
        ]
