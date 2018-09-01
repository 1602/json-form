port module CustomElement exposing (init, subscriptions, update, view)

--import Html.Attributes exposing (class, classList)
--import Html.Events exposing (onClick)

import Html exposing (Html, div, h3, text)
import Json.Decode exposing (Value, decodeValue)
import Json.Encode as Encode
import Json.Form
import Json.Schema.Definitions
import Json.Value exposing (decoder)


type alias Model =
    { form : Json.Form.Model
    , schema : Json.Schema.Definitions.Schema
    , editedValue : Maybe Json.Value.JsonValue
    }


init : Value -> ( Model, Cmd Msg )
init v =
    let
        schema =
            v
                |> decodeValue (Json.Decode.at [ "schema" ] Json.Schema.Definitions.decoder)
                |> Result.mapError (Json.Decode.errorToString >> Debug.log)
                |> Result.withDefault Json.Schema.Definitions.blankSchema

        valueLocal =
            v
                |> decodeValue (Json.Decode.at [ "value" ] Json.Value.decoder)
                |> Result.toMaybe
    in
    ( { form = Json.Form.init schema valueLocal
      , editedValue = valueLocal
      , schema = schema
      }
    , Cmd.none
    )


type Msg
    = JsonFormMsg Json.Form.Msg
    | ChangeValue Value
    | ChangeSchema Value


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ChangeSchema v ->
            let
                schema =
                    v
                        |> decodeValue Json.Schema.Definitions.decoder
                        |> Result.withDefault Json.Schema.Definitions.blankSchema
            in
            ( { model
                | schema = schema
                , form = Json.Form.init schema model.editedValue
              }
            , Cmd.none
            )

        ChangeValue v ->
            let
                valueLocal =
                    v
                        |> decodeValue Json.Value.decoder
                        |> Result.toMaybe
            in
            ( { model
                | editedValue = valueLocal
                , form = Json.Form.init model.schema valueLocal
              }
            , Cmd.none
            )

        JsonFormMsg msg ->
            let
                ( ( m, cmd ), exMsg ) =
                    Json.Form.update msg model.form

                ( editedValue, exCmd ) =
                    case exMsg of
                        Json.Form.UpdateValue v isValid ->
                            ( v
                            , Encode.object
                                [ ( "value"
                                  , v
                                        |> Maybe.withDefault Json.Value.NullValue
                                        |> Json.Value.encode
                                  )
                                , ( "isValid", Encode.bool isValid )
                                ]
                                |> value
                            )

                        _ ->
                            ( model.editedValue, Cmd.none )
            in
            ( { model
                | form = m
                , editedValue = editedValue
              }
            , Cmd.batch [ cmd |> Cmd.map JsonFormMsg, exCmd ]
            )


view : Model -> Html Msg
view model =
    model.form
        |> Json.Form.view
        |> Html.map JsonFormMsg


port valueChange : (Value -> msg) -> Sub msg


port value : Value -> Cmd msg


port schemaChange : (Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ valueChange ChangeValue
        , schemaChange ChangeSchema
        ]
