module StatefulComponent.Form exposing (Model, Msg, init, update, view)

import Dict
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode exposing (Value)
import JsonValue exposing (JsonValue(..))
import Json.Schema.Definitions as Schema
    exposing
        ( Schemata(Schemata)
        , Schema(ObjectSchema, BooleanSchema)
        , blankSchema
        , blankSubSchema
        )
import Element.Events as Events exposing (onInput, onClick)
import FeatherIcons as Icons
import Element.Attributes as Attributes
    exposing
        ( center
        , verticalCenter
        , vary
        , inlineStyle
        , spacing
        , padding
        , paddingLeft
        , height
        , minWidth
        , width
        , fill
        , px
        , percent
        )
import Element exposing (Element, el, row, text, column, paragraph, empty)
import Styles
    exposing
        ( Styles
            ( None
            , Main
            , SourceCode
            , TextInput
            )
        , Variations
        , stylesheet
        )


type alias View =
    Element Styles Variations Msg


type alias Path =
    List String


type Msg
    = NoOp
    | StringInput Path String
    | DeletePath Path


type alias Model =
    { value : JsonValue
    , schema : Schema
    , expandedNodes : List (List String)
    }


init : Schema -> Value -> Model
init schema value =
    { value =
        value
            |> Debug.log "incoming value"
            |> decodeValue JsonValue.decoder
            |> Result.withDefault JsonValue.NullValue
    , schema = schema
    , expandedNodes = [ [] ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        StringInput path str ->
            { model
                | value =
                    model.value
                        |> JsonValue.setIn path (StringValue str)
                        |> Result.mapError (Debug.log "StringInput")
                        |> Result.withDefault model.value
            }
                ! []

        DeletePath path ->
            { model
                | value =
                    model.value
                        |> JsonValue.deleteIn path
                        |> Result.mapError (Debug.log "DeletePath")
                        |> Result.withDefault model.value
            }
                ! []


view : Model -> View
view model =
    viewValue model model.schema model.value []


delete : Path -> View
delete path =
    Icons.xCircle
        |> Icons.withStrokeWidth 1
        |> Icons.withSize 18
        |> Icons.toHtml []
        |> Element.html
        |> el None
            [ onClick <| DeletePath path
            , width <| px 18
            , height <| px 18
            ]


viewObject : Model -> Schema -> List ( String, JsonValue ) -> Path -> View
viewObject model schema props path =
    let
        viewProperty key subSchema value =
            let
                deeperLevelPath =
                    path ++ [ key ]

                x =
                    Debug.log key subSchema
            in
                column None
                    []
                    [ row None
                        [ verticalCenter, spacing 5, width <| px 200 ]
                        [ text key
                        , delete deeperLevelPath
                        ]
                    , displayDescription subSchema
                    , if List.member deeperLevelPath model.expandedNodes then
                        row None
                            []
                            [ viewValue model subSchema value deeperLevelPath
                            ]
                      else
                        empty
                    ]

        iterateOverSchemata propsDict (Schemata schemata) =
            schemata
                |> List.filterMap
                    (\( propName, subSchema ) ->
                        propsDict
                            |> Dict.get propName
                            |> Maybe.map (viewProperty propName subSchema)
                    )
                |> column None [ paddingLeft 10 ]

        iterateOverProps list schema =
            list
                |> List.map (\( key, value ) -> viewProperty key schema value)
                |> column None [ paddingLeft 10 ]
    in
        case schema of
            BooleanSchema True ->
                iterateOverProps props blankSchema

            BooleanSchema False ->
                iterateOverProps props <| ObjectSchema { blankSubSchema | not = Just blankSchema }

            ObjectSchema os ->
                os.properties
                    |> Maybe.map (iterateOverSchemata (Dict.fromList props))
                    |> Maybe.withDefault empty


displayDescription : Schema -> View
displayDescription schema =
    case schema of
        ObjectSchema os ->
            os.description
                |> Maybe.map text
                |> Maybe.map (el None [])
                |> Maybe.withDefault empty

        _ ->
            empty


viewArray : Model -> Schema -> List JsonValue -> Path -> View
viewArray model schema list path =
    list
        |> List.indexedMap
            (\index value ->
                let
                    deeperLevelPath =
                        path ++ [ toString index ]

                    -- TODO use subschema for property
                    schema =
                        model.schema
                in
                    column None
                        []
                        [ row None
                            [ verticalCenter, spacing 5, width <| px 200 ]
                            [ text <| toString index
                            , delete deeperLevelPath
                            ]
                        , displayDescription schema
                        , row None
                            [ verticalCenter, spacing 5, width <| fill 1 ]
                            [ viewValue model schema value deeperLevelPath
                            ]
                        ]
            )
        |> column None [ paddingLeft 10 ]


viewString : Model -> Schema -> String -> Path -> View
viewString model schema stringValue path =
    row None
        []
        [ stringValue
            |> Element.inputText TextInput [ onInput <| StringInput path ]
        ]


viewValue : Model -> Schema -> JsonValue -> Path -> View
viewValue model schema value path =
    case value of
        ObjectValue ov ->
            viewObject model schema ov path

        ArrayValue av ->
            viewArray model schema av path

        StringValue sv ->
            viewString model schema sv path

        _ ->
            text "something else"
