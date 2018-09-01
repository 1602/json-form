module Json.Form exposing
    ( ExternalMsg(..)
    , Model
    , Msg
    , init
    , update
    , view
    )

import Dict exposing (Dict)
import ErrorMessages exposing (stringifyError)
import Html exposing (..)
import Json.Decode as Decode exposing (decodeValue)
import Json.Form.Definitions as Definitions exposing (EditingMode(..), Msg(..), Path)
import Json.Form.Selection as Selection
import Json.Form.TextField as TextField
import Json.Form.UiSpec as UiSpec
import Json.Schema
import Json.Schema.Definitions exposing (..)
import Json.Schema.Validation exposing (Error)
import Json.Value exposing (JsonValue(..))
import Util exposing (..)


type ExternalMsg
    = None
    | UpdateValue (Maybe JsonValue) Bool


type alias Model =
    Definitions.Model


type alias Msg =
    Definitions.Msg


init : Schema -> Maybe JsonValue -> Model
init =
    Definitions.init


view : Model -> Html Msg
view model =
    viewNode model model.schema False []


viewNode : Model -> Schema -> Bool -> Path -> Html Msg
viewNode model schema isRequired path =
    case editingMode model schema of
        TextField ->
            TextField.view model schema isRequired path

        NumberField ->
            TextField.viewNumeric model schema isRequired path

        Switch ->
            Selection.switch model schema isRequired path

        Checkbox ->
            Selection.checkbox model schema isRequired path

        Object ->
            viewObject model schema isRequired path

        x ->
            text "not implemented"


editingMode : Model -> Schema -> EditingMode
editingMode model schema =
    case schema of
        ObjectSchema os ->
            case os.type_ of
                SingleType NumberType ->
                    NumberField

                SingleType StringType ->
                    TextField

                SingleType BooleanType ->
                    getBooleanUiWidget schema

                SingleType ObjectType ->
                    Object

                _ ->
                    JsonEditor

        _ ->
            JsonEditor


getBooleanUiWidget : Schema -> EditingMode
getBooleanUiWidget schema =
    case schema |> getUiSpec of
        UiSpec.Switch ->
            Switch

        _ ->
            Checkbox


viewObject : Model -> Schema -> Bool -> Path -> Html Msg
viewObject model schema isRequired path =
    let
        iterateOverSchemata propsDict required (Schemata schemata) =
            schemata
                |> List.map
                    (\( propName, subSchema ) ->
                        viewNode model subSchema (required |> Maybe.withDefault [] |> List.member propName) (path ++ [ propName ])
                    )
    in
    case schema of
        ObjectSchema os ->
            os.properties
                |> Maybe.map (iterateOverSchemata Dict.empty os.required)
                |> Maybe.withDefault []
                |> div []

        _ ->
            text ""


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        FocusInput focused ->
            ( ( { model
                    | focused = focused
                    , beingEdited = touch focused model.focused model.beingEdited
                }
              , Cmd.none
              )
            , None
            )

        FocusNumericInput focused ->
            case focused of
                Nothing ->
                    editValue
                        { model | beingEdited = touch focused model.focused model.beingEdited }
                        (model.focused |> Maybe.withDefault [])
                        (case model.editedNumber |> String.toFloat of
                            Just num ->
                                Json.Value.NumericValue num

                            _ ->
                                Json.Value.StringValue model.editedNumber
                        )

                Just somePath ->
                    ( ( { model
                            | focused = focused
                            , editedNumber =
                                model.value
                                    |> Maybe.map (Json.Value.getIn somePath)
                                    |> Maybe.andThen Result.toMaybe
                                    |> Maybe.map jsonValueToString
                                    |> Maybe.withDefault ""
                        }
                      , Cmd.none
                      )
                    , None
                    )

        EditValue path val ->
            editValue model path val

        EditNumber str ->
            case str |> String.toFloat of
                Just num ->
                    editValue { model | editedNumber = str } (model.focused |> Maybe.withDefault []) (Json.Value.NumericValue num)

                _ ->
                    ( ( { model | editedNumber = str }
                      , Cmd.none
                      )
                    , None
                    )


touch : Maybe Path -> Maybe Path -> List Path -> List Path
touch path focused beingEdited =
    if path == Nothing then
        beingEdited
            |> (::) (focused |> Maybe.withDefault [])

    else
        beingEdited


editValue : Model -> Path -> JsonValue -> ( ( Model, Cmd Msg ), ExternalMsg )
editValue model path val =
    let
        updatedJsonValue =
            model.value
                |> Maybe.withDefault Json.Value.NullValue
                |> Json.Value.setIn path val
                |> Result.toMaybe
                |> Maybe.withDefault Json.Value.NullValue

        updatedValue =
            updatedJsonValue
                |> Json.Value.encode

        validationResult =
            model.schema
                |> Json.Schema.validateValue { applyDefaults = True } updatedValue
    in
    case validationResult of
        Ok v ->
            ( ( { model
                    | value =
                        v
                            |> decodeValue Json.Value.decoder
                            |> Result.toMaybe
                    , errors = Dict.empty
                }
              , Cmd.none
              )
            , UpdateValue (Just updatedJsonValue) True
            )

        Err e ->
            ( ( { model
                    | value = Just updatedJsonValue
                    , errors = dictFromListErrors e
                }
              , Cmd.none
              )
            , UpdateValue (Just updatedJsonValue) False
            )


dictFromListErrors : List Error -> Dict Path (List String)
dictFromListErrors list =
    list
        |> List.foldl
            (\error dict ->
                dict
                    |> Dict.update error.jsonPointer.path
                        (\listDetails ->
                            (case listDetails of
                                Just l ->
                                    l ++ [ error.details |> stringifyError ]

                                Nothing ->
                                    [ error.details |> stringifyError ]
                            )
                                |> Just
                        )
            )
            Dict.empty
