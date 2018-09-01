module Json.Form.Selection exposing (checkbox, switch)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onCheck, onFocus, onInput)
import Json.Form.Definitions exposing (..)
import Json.Form.Helper as Helper
import Json.Schema.Definitions exposing (Schema, getCustomKeywordValue)
import Json.Value exposing (JsonValue(..))
import Util exposing (..)


switch : Model -> Schema -> Bool -> Path -> Html Msg
switch model schema isRequired path =
    let
        isChecked =
            case model.value |> Maybe.andThen (Json.Value.getIn path >> Result.toMaybe) of
                Just (BoolValue x) ->
                    x

                _ ->
                    False

        ( hasError, helperText ) =
            Helper.view model schema path
    in
    label
        [ classList
            [ ( "jf-switch", True )
            , ( "jf-switch--on", isChecked )
            , ( "jf-switch--focused", model.focused |> Maybe.map ((==) path) |> Maybe.withDefault False )
            , ( "jf-switch--invalid", hasError )
            ]
        ]
        [ input
            [ type_ "checkbox"
            , class "jf-switch__input"
            , checked isChecked
            , onFocus <| FocusInput (Just path)
            , onBlur <| FocusInput Nothing
            , onCheck <| (Json.Value.BoolValue >> EditValue path)
            ]
            []
        , span [ class "jf-switch__label" ] [ schema |> getTitle isRequired |> text ]
        , div [ class "jf-switch__track" ] []
        , div [ class "jf-switch__thumb" ] []
        , div [ class "jf-switch__helper-text" ] [ helperText ]
        ]


checkbox : Model -> Schema -> Bool -> Path -> Html Msg
checkbox model schema isRequired path =
    let
        isChecked =
            case model.value |> Maybe.andThen (Json.Value.getIn path >> Result.toMaybe) of
                Just (BoolValue x) ->
                    x

                _ ->
                    False

        ( hasError, helperText ) =
            Helper.view model schema path
    in
    label
        [ classList
            [ ( "jf-checkbox", True )
            , ( "jf-checkbox--on", isChecked )
            , ( "jf-checkbox--focused", model.focused |> Maybe.map ((==) path) |> Maybe.withDefault False )
            , ( "jf-checkbox--invalid", hasError )
            ]
        ]
        [ input
            [ type_ "checkbox"
            , class "jf-checkbox__input"
            , checked isChecked
            , onFocus <| FocusInput (Just path)
            , onBlur <| FocusInput Nothing
            , onCheck <| (Json.Value.BoolValue >> EditValue path)
            ]
            []
        , span [ class "jf-checkbox__label" ] [ schema |> getTitle isRequired |> text ]
        , div [ class "jf-checkbox__box-outline" ]
            [ div [ class "jf-checkbox__tick-outline" ] []
            ]
        , div [ class "jf-checkbox__helper-text" ] [ helperText ]
        ]
