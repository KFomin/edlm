module Main exposing (..)

import Browser
import Html exposing (Html)
import Report


main : Program {} Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { reportModel : Report.Model
    }


init : {} -> ( Model, Cmd msg )
init _ =
    ( { reportModel =
            { report = Report.NotAsked
            , reportHasHeaders = False
            , reportColumns = []
            , expanded = Nothing
            }
      }
    , Cmd.none
    )


type Msg
    = ToReportMsg Report.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToReportMsg reportMsg ->
            let
                ( reportModel, reportCmd ) =
                    Report.update reportMsg model.reportModel
            in
            ( { model | reportModel = reportModel }
            , Cmd.map ToReportMsg reportCmd
            )


view : Model -> Browser.Document Msg
view model =
    { title = "everyday life management"
    , body =
        [ Report.view model.reportModel
            |> Html.map ToReportMsg
        ]
    }
