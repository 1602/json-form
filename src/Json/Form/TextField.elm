module Json.Form.TextField exposing (view, viewNumeric)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onFocus, onInput)
import Icons exposing (deleteIcon, errorIcon, eye, eyeOff)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Json.Form.Config exposing (TextFieldStyle(..))
import Json.Form.Definitions exposing (..)
import Json.Form.Helper as Helper
import Json.Form.UiSpec exposing (Rule(..), UiSpec, Widget(..), applyRule)
import Json.Schema.Definitions exposing (Schema(..), getCustomKeywordValue)
import Json.Value as JsonValue exposing (JsonValue(..))
import JsonFormUtil as Util exposing (getTitle, getUiSpec, jsonValueToString)


view : Model -> Schema -> Bool -> Bool -> Bool -> Path -> Html Msg
view model schema isJson isRequired isDisabled path =
    let
        id =
            (model.config.name ++ "_") ++ (path |> String.join "_")

        ( enum, examples ) =
            case schema of
                ObjectSchema os ->
                    ( os.enum
                        |> Maybe.map (List.map (\v -> v |> Decode.decodeValue Decode.string |> Result.withDefault ""))
                    , os.examples
                        |> Maybe.map (List.map (\v -> v |> Decode.decodeValue Decode.string |> Result.withDefault ""))
                    )

                _ ->
                    ( Nothing, Nothing )

        isFocused =
            model.focused
                |> Maybe.map ((==) path)
                |> Maybe.withDefault False

        editedValue =
            if isJson then
                if isFocused then
                    model.editedJson

                else
                    model.value
                        |> Maybe.withDefault (ObjectValue [])
                        |> JsonValue.getIn path
                        |> Result.toMaybe
                        |> Maybe.map (JsonValue.encode >> Encode.encode 4)
                        |> Maybe.withDefault ""

            else
                model.value
                    |> Maybe.map (JsonValue.getIn path)
                    |> Maybe.andThen Result.toMaybe
                    |> Maybe.map jsonValueToString
                    |> Maybe.withDefault ""

        ( hasError, helperText ) =
            Helper.view model schema path

        uiSpec =
            schema |> getUiSpec

        isPassword =
            uiSpec.widget == Just PasswordField

        multilineConfig =
            case uiSpec.widget of
                Just (MultilineTextField conf) ->
                    Just conf

                _ ->
                    if isJson then
                        Just { minRows = 5, maxRows = 8 }

                    else
                        Nothing

        ( disabled, hidden ) =
            applyRule model.value path uiSpec.rule

        actuallyDisabled =
            isDisabled || disabled

        icon =
            if isPassword then
                if model.showPassword then
                    ToggleShowPassword |> eyeOff |> Just

                else
                    ToggleShowPassword |> eye |> Just

            else if hasError && model.config.showErrorIcon then
                Just errorIcon

            else if not isRequired && editedValue /= "" && not actuallyDisabled then
                DeleteProperty path |> deleteIcon |> Just

            else
                Nothing

        baseAttributes =
            [ class "jf-textfield__input"
            , value <| editedValue
            , Html.Attributes.id id
            , Html.Attributes.name id
            , Html.Attributes.autocomplete False
            , Html.Attributes.disabled actuallyDisabled
            ]
                ++ (if enum /= Nothing || examples /= Nothing then
                        [ Html.Attributes.list <| id ++ "_enum" ]

                    else
                        []
                   )

        editMultiline : (Float -> String -> msg) -> Decoder msg
        editMultiline fn =
            Decode.map2 fn
                (Decode.at [ "target", "scrollHeight" ] Decode.float)
                (Decode.at [ "target", "value" ] Decode.string)

        isOutlined =
            model.config.textFieldStyle == Outlined

        textInput =
            case multilineConfig of
                Just mlConf ->
                    let
                        paddings =
                            if model.config.dense then
                                if isOutlined then
                                    13 + 13

                                else
                                    22 + 13

                            else
                                20 + 17

                        rows =
                            case model.fieldHeights |> Dict.get path of
                                Just height ->
                                    Basics.min ((height - paddings) / 18 |> round) mlConf.maxRows

                                Nothing ->
                                    mlConf.minRows
                    in
                    if isJson then
                        textarea
                            (Html.Events.on "input" (editMultiline <| EditJson path)
                                :: Html.Attributes.rows rows
                                :: (onFocus <| FocusFragileInput False (Just path))
                                :: (onBlur <| FocusFragileInput False Nothing)
                                :: baseAttributes
                            )
                            []

                    else
                        textarea
                            (Html.Events.on "input" (editMultiline <| EditMultiline path)
                                :: Html.Attributes.rows rows
                                :: (onFocus <| FocusInput (Just path))
                                :: (onBlur <| FocusInput Nothing)
                                :: baseAttributes
                            )
                            []

                Nothing ->
                    input
                        (baseAttributes
                            ++ [ if isPassword && not model.showPassword then
                                    type_ "password"

                                 else
                                    type_ "text"
                               , onInput (JsonValue.StringValue >> EditValue path)
                               , onFocus <| FocusInput (Just path)
                               , onBlur <| FocusInput Nothing
                               ]
                        )
                        []
    in
    div
        [ classList
            [ ( "jf-element", True )
            , ( "jf-element--hidden", hidden )
            , ( "jf-element--invalid", hasError )
            ]
        ]
        [ div
            [ classList
                [ ( "jf-textfield", True )
                , ( "jf-textfield--outlined", isOutlined )
                , ( "jf-textfield--dense", model.config.dense )
                , ( "jf-textfield--focused", model.focused |> Maybe.map ((==) path) |> Maybe.withDefault False )
                , ( "jf-textfield--empty", editedValue == "" )
                , ( "jf-textfield--invalid", hasError )
                , ( "jf-textfield--has-icon", icon /= Nothing )
                , ( "jf-textfield--disabled", actuallyDisabled )
                , ( "jf-textfield--multiline", multilineConfig /= Nothing )
                , ( "jf-textfield--json", isJson )
                ]
            ]
            -- , onFocus <| FocusTextInput path
            -- , Html.Attributes.tabindex -1
            [ label [ class "jf-textfield__label" ] [ schema |> getTitle isRequired |> text ]
            , textInput
            , icon |> Maybe.withDefault (text "")
            ]
        , div [ class "jf-helper-text" ] [ helperText ]
        , case enum of
            Just listStrings ->
                listStrings
                    |> List.map (\s -> Html.option [ Html.Attributes.value s ] [])
                    |> Html.datalist [ Html.Attributes.id <| id ++ "_enum" ]

            Nothing ->
                case examples of
                    Just listStrings ->
                        listStrings
                            |> List.map (\s -> Html.option [ Html.Attributes.value s ] [])
                            |> Html.datalist [ Html.Attributes.id <| id ++ "_enum" ]

                    Nothing ->
                        text ""
        ]


viewNumeric : Model -> Schema -> Bool -> Bool -> Path -> Html Msg
viewNumeric model schema isRequired isDisabled path =
    let
        id =
            path |> String.join "_"

        isFocused =
            model.focused
                |> Maybe.map ((==) path)
                |> Maybe.withDefault False

        editedValue =
            if isFocused then
                model.editedJson

            else
                model.value
                    |> Maybe.map (JsonValue.getIn path)
                    |> Maybe.andThen Result.toMaybe
                    |> Maybe.map Util.jsonValueToString
                    |> Maybe.withDefault ""

        ( hasError, helperText ) =
            Helper.view model schema path

        uiSpec =
            schema |> getUiSpec

        ( disabled, hidden ) =
            applyRule model.value path uiSpec.rule

        actuallyDisabled =
            isDisabled || disabled

        icon =
            if hasError then
                errorIcon |> Just

            else if not isRequired && editedValue /= "" && not actuallyDisabled then
                DeleteProperty path |> deleteIcon |> Just

            else
                Nothing

        numericInput =
            input
                [ class "jf-textfield__input"
                , onFocus <| FocusFragileInput True (Just path)
                , onBlur <| FocusFragileInput True Nothing
                , onInput <| EditNumber
                , Html.Attributes.id id
                , Html.Attributes.name id
                , value <| editedValue
                , type_ "number"
                , Html.Attributes.disabled actuallyDisabled
                ]
                []
    in
    div
        [ classList
            [ ( "jf-element", True )
            , ( "jf-element--hidden", hidden )
            , ( "jf-element--invalid", hasError )
            ]
        ]
        [ div
            [ classList
                [ ( "jf-textfield", True )
                , ( "jf-textfield--outlined", model.config.textFieldStyle == Outlined )
                , ( "jf-textfield--dense", model.config.dense )
                , ( "jf-textfield--focused", isFocused )
                , ( "jf-textfield--empty", editedValue == "" )
                , ( "jf-textfield--invalid", hasError )
                , ( "jf-textfield--has-icon", True )
                , ( "jf-textfield--disabled", actuallyDisabled )
                , ( "jf-textfield--hidden", hidden )
                ]
            ]
            [ label [ class "jf-textfield__label" ] [ schema |> getTitle isRequired |> text ]
            , numericInput
            , icon |> Maybe.withDefault (text "")
            ]
        , div [ class "jf-helper-text" ] [ helperText ]
        ]
