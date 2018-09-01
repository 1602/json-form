module Json.Form.Helper exposing (view)

import Dict exposing (Dict)
import Html exposing (Html, text)
import Json.Form.Definitions exposing (..)
import Json.Schema.Definitions exposing (Schema)
import Util exposing (..)


view : Model -> Schema -> Path -> ( Bool, Html msg )
view model schema path =
    let
        errors =
            model.errors
                |> Dict.get path

        hasError =
            errors /= Nothing && List.member path model.beingEdited
    in
    ( hasError
    , if hasError then
        errors
            |> Maybe.withDefault []
            |> String.join ", "
            |> (++) "Error: "
            |> text

      else
        schema
            |> getDescription
            |> text
    )
