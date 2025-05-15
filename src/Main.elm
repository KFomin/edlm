module Main exposing (..)

import Browser
import File exposing (File)
import File.Select as Select
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import Task


main : Program {} Model Msg
main =
    Browser.document
        { init =
            \_ ->
                ( { report =
                        { headers = []
                        , rows = []
                        , filtered = []
                        }
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { report :
        { headers : List String
        , rows : List (List String)
        , filtered : List (List String)
        }
    }


type Msg
    = FileRequested
    | FileSelected File
    | FileLoaded String
    | ReportFiltered String
    | HeaderClicked Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ report } as model) =
    case msg of
        FileRequested ->
            ( model
            , Select.file [ "text/csv" ] FileSelected
            )

        FileSelected file ->
            ( model
            , Task.perform FileLoaded (File.toString file)
            )

        FileLoaded content ->
            ( { model | report = decodeBankReport content }
            , Cmd.none
            )

        ReportFiltered search ->
            ( { model
                | report =
                    { report
                        | filtered =
                            report.rows
                                |> List.filter
                                    (\row ->
                                        row
                                            |> List.any
                                                (\cell ->
                                                    String.contains
                                                        (String.toLower search)
                                                        (String.toLower cell)
                                                        |> (\a ->
                                                                let
                                                                    _ =
                                                                        Debug.log "a: " a
                                                                in
                                                                a
                                                           )
                                                )
                                    )
                    }
              }
            , Cmd.none
            )

        HeaderClicked index ->
            ( { model
                | report =
                    { report
                        | filtered =
                            report.filtered
                                |> List.sortBy
                                    (\row ->
                                        row
                                            |> List.take (index + 1)
                                            |> List.reverse
                                            |> List.head
                                            |> Maybe.withDefault ""
                                    )
                    }
              }
            , Cmd.none
            )


decodeBankReport : String -> { headers : List String, rows : List (List String), filtered : List (List String) }
decodeBankReport string =
    let
        report =
            toReport string

        _ =
            Debug.log "report: " report
    in
    { headers = report.headers
    , rows = report.rows
    , filtered = report.rows
    }


toReport : String -> { headers : List String, rows : List (List String) }
toReport stringReport =
    let
        maybeHeaders : Maybe (List String)
        maybeHeaders =
            stringReport
                |> String.split "\u{000D}\n"
                |> List.head
                |> Maybe.map
                    (\headers ->
                        String.split ";" headers
                            |> List.map (String.replace "\"" "")
                    )

        maybeRows : Maybe (List (List String))
        maybeRows =
            stringReport
                |> String.split "\u{000D}\n"
                |> List.tail
                |> Maybe.map
                    (\rows ->
                        rows
                            |> List.map
                                (\row ->
                                    row
                                        |> String.replace "\"" ""
                                        |> String.split ";"
                                )
                    )
    in
    { headers = Maybe.withDefault [] maybeHeaders
    , rows = Maybe.withDefault [] maybeRows
    }


view : Model -> Browser.Document Msg
view { report } =
    { title = "everyday life management"
    , body =
        [ Html.div []
            [ Html.input
                [ Attrs.type_ "button"
                , Attrs.value "upload file"
                , Attrs.class "p-2 m-2 border rounded"
                , Events.onClick FileRequested
                ]
                []
            , Html.input
                [ Attrs.type_ "text"
                , Attrs.class "p-2 m-2 border rounded"
                , Attrs.placeholder "search"
                , Events.onInput ReportFiltered
                ]
                []
            ]
        , Html.table
            [ Attrs.class "table-auto border-collapse border border-gray-400 max-100 w-full" ]
            ([ Html.tr
                []
                (List.indexedMap
                    (\index header ->
                        Html.th
                            [ Attrs.class "border border-gray-300 p-2", Events.onClick (HeaderClicked index) ]
                            [ Html.text header ]
                    )
                    report.headers
                )
             ]
                ++ List.map
                    (\row ->
                        Html.tr
                            []
                            (List.map
                                (\cell ->
                                    Html.td
                                        [ Attrs.class "border border-gray-300 p-2" ]
                                        [ Html.text cell ]
                                )
                                row
                            )
                    )
                    report.filtered
            )
        ]
    }
