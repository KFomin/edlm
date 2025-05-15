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
                ( { report = { headers = [], rows = [] } }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { report : { headers : List String, rows : List (List String) } }


type Msg
    = FileRequested
    | FileSelected File
    | FileLoaded String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
            let
                _ =
                    Debug.log "do decode: " (decodeBankReport content)
            in
            ( { model | report = decodeBankReport content }
            , Cmd.none
            )


decodeBankReport : String -> { headers : List String, rows : List (List String) }
decodeBankReport string =
    let
        report =
            toReport string

        _ =
            Debug.log "report: " report
    in
    { headers = report.headers
    , rows = report.rows
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
        [ Html.input
            [ Attrs.type_ "button"
            , Attrs.value "upload file"
            , Events.onClick FileRequested
            ]
            []
        , Html.table
            [ Attrs.class "table-auto border-collapse border border-gray-400 max-100" ]
            ([ Html.tr
                []
                (List.map
                    (\header ->
                        Html.th
                            [ Attrs.class "border border-gray-300 p-2" ]
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
                    report.rows
            )
        ]
    }
