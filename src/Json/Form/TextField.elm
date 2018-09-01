module Json.Form.TextField exposing (view, viewNumeric)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onFocus, onInput)
import Json.Form.Definitions exposing (..)
import Json.Form.Helper as Helper
import Json.Form.UiSpec exposing (UiSpec(..))
import Json.Schema.Definitions exposing (Schema, getCustomKeywordValue)
import Json.Value exposing (JsonValue)
import Util exposing (..)


view : Model -> Schema -> Bool -> Path -> Html Msg
view model schema isRequired path =
    let
        editedValue =
            model.value
                |> Maybe.map (Json.Value.getIn path)
                |> Maybe.andThen Result.toMaybe
                |> Maybe.map jsonValueToString
                |> Maybe.withDefault ""

        ( hasError, helperText ) =
            Helper.view model schema path

        isPassword =
            case schema |> getUiSpec of
                PasswordField ->
                    True

                _ ->
                    False
    in
    div
        [ classList
            [ ( "jf-textfield", True )
            , ( "jf-textfield--focused", model.focused |> Maybe.map ((==) path) |> Maybe.withDefault False )
            , ( "jf-textfield--empty", editedValue == "" )
            , ( "jf-textfield--invalid", hasError )
            ]
        ]
        [ input
            [ class "jf-textfield__input"
            , onFocus <| FocusInput (Just path)
            , onBlur <| FocusInput Nothing
            , onInput <| \str -> EditValue path (Json.Value.StringValue str)
            , value <| editedValue
            , if isPassword then
                type_ "password"

              else
                type_ "text"
            ]
            []
        , label [ class "jf-textfield__label" ] [ schema |> getTitle isRequired |> text ]
        , div [ class "jf-textfield__helper-text" ] [ helperText ]
        ]


viewNumeric : Model -> Schema -> Bool -> Path -> Html Msg
viewNumeric model schema isRequired path =
    let
        isFocused =
            model.focused
                |> Maybe.map ((==) path)
                |> Maybe.withDefault False

        editedValue =
            if isFocused then
                model.editedNumber

            else
                model.value
                    |> Maybe.map (Json.Value.getIn path)
                    |> Maybe.andThen Result.toMaybe
                    |> Maybe.map Util.jsonValueToString
                    |> Maybe.withDefault ""

        ( hasError, helperText ) =
            Helper.view model schema path
    in
    div
        [ classList
            [ ( "jf-textfield", True )
            , ( "jf-textfield--focused", isFocused )
            , ( "jf-textfield--empty", editedValue == "" )
            , ( "jf-textfield--invalid", hasError )
            ]
        ]
        [ input
            [ class "jf-textfield__input"
            , onFocus <| FocusNumericInput (Just path)
            , onBlur <| FocusNumericInput Nothing
            , onInput <| EditNumber
            , value <| editedValue
            , type_ "number"
            ]
            []
        , label [ class "jf-textfield__label" ] [ schema |> getTitle isRequired |> text ]
        , div [ class "jf-textfield__helper-text" ] [ helperText ]
        ]
