module StatefulComponent.Form exposing
    ( ExternalMsg(..)
    , FormOptions
    , Model
    , Msg
    , defaultOptions
    , init
    , update
    , updateSchema
    , updateValue
    , view
    )

import Dict exposing (Dict)
import Dom
import Element exposing (Element, column, el, empty, paragraph, row, text)
import Element.Attributes as Attributes
    exposing
        ( center
        , class
        , fill
        , height
        , inlineStyle
        , minWidth
        , padding
        , paddingBottom
        , paddingLeft
        , paddingRight
        , paddingTop
        , percent
        , px
        , spacing
        , tabindex
        , vary
        , verticalCenter
        , width
        )
import Element.Events as Events exposing (onBlur, onClick, onFocus, onInput)
import ErrorMessages exposing (stringifyError)
import FeatherIcons as Icons
import Json.Decode as Decode exposing (Decoder, decodeValue)
import Json.Encode as Encode exposing (Value)
import Json.Schema
import Json.Schema.Definitions as Schema
    exposing
        ( Items(..)
        , Schema(..)
        , Schemata(..)
        , SingleType(..)
        , Type(..)
        , blankSchema
        , blankSubSchema
        )
import Json.Schema.Validation exposing (Error, ValidationError)
import JsonValue exposing (JsonValue(..), getIn)
import Ref
import Styles
    exposing
        ( Styles(..)
        , Variations(..)
        , stylesheet
        )
import Task


type alias View =
    Element Styles Variations Msg


type alias Path =
    List String


type Msg
    = NoMsg
    | ValueInput Path String
    | StringInput Path String
    | NumericInput Path String
    | BoolInput Path Bool
    | DeletePath Path
    | AddItem Path
    | AddProperty Path
    | SetEditPropertyName String Path Int
    | EditPropertyName String
    | StopEditingPropertyName
    | ExpandNode Path
    | CollapseNode Path
    | OpenMenu Path
    | CloseMenu
    | FocusInput Path Schema
    | BlurInput Path


type ExternalMsg
    = NoOp
    | UpdateValue Value
    | SaveExpandedNodes (List Path)


type alias Model =
    { value : JsonValue
    , schema : Schema
    , validationErrors : Dict Path (List String)
    , options : FormOptions
    , menu : Maybe Path
    , focusInput : Path
    , editingNow : String
    , editingSchema : Maybe Schema
    , edited : Dict Path Bool
    , editPropPath : Path
    , editPropIndex : Maybe Int
    , editPropName : String
    }


type alias FormOptions =
    { schema : Schema
    , expandedNodes : List Path
    , applyDefaults : Bool
    , showEmptyOptionalProps : Bool
    , showInitialValidationErrors : Bool
    , useTitleAsLabel : Bool
    , allowExpandingNodes :
        Bool

    --, monospaceTitle : Bool
    }


defaultOptions : FormOptions
defaultOptions =
    { schema = blankSchema
    , expandedNodes = [ [] ]
    , applyDefaults = False
    , showEmptyOptionalProps = False
    , showInitialValidationErrors = False
    , useTitleAsLabel = False
    , allowExpandingNodes =
        True

    --, monospaceTitle = AlwaysMonospace | MonospaceWhenKeyUsedAsLabel | NeverMonospace
    }


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


init : FormOptions -> Value -> Model
init formOptions v =
    let
        validationResult =
            Json.Schema.validateValue { applyDefaults = formOptions.applyDefaults } v formOptions.schema

        ( value, validationErrors ) =
            case validationResult of
                Ok validValue ->
                    ( validValue, Dict.empty )

                Err list ->
                    ( v, list |> dictFromListErrors )

        blankModel =
            { schema = formOptions.schema
            , value =
                value
                    |> decodeValue JsonValue.decoder
                    |> Result.withDefault JsonValue.NullValue
            , validationErrors = validationErrors
            , options = formOptions
            , menu = Nothing
            , focusInput = []
            , editingNow = ""
            , editingSchema = Nothing
            , edited = Dict.empty
            , editPropPath = []
            , editPropIndex = Nothing
            , editPropName = ""
            }
    in
    blankModel


updateValue : Value -> Model -> Model
updateValue v m =
    { m
        | value =
            v
                |> decodeValue JsonValue.decoder
                |> Result.withDefault JsonValue.NullValue
    }


updateSchema : Schema -> Model -> Model
updateSchema s m =
    { m | schema = s }


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        NoMsg ->
            ( model
            , Cmd.none
            )
                => NoOp

        ValueInput path str ->
            let
                updatedValue =
                    str
                        |> Decode.decodeString JsonValue.decoder
                        |> Result.andThen
                            (\v ->
                                model.value
                                    |> JsonValue.setIn path v
                                    --  TODO display setIn error
                                    |> Result.mapError (Debug.log "ValueInput.setIn")
                            )
                        --  TODO display parse error
                        |> Result.mapError (Debug.log "ValueInput.parse")
                        |> Result.withDefault model.value

                encodedValue =
                    updatedValue |> JsonValue.encode

                validationResult =
                    Json.Schema.validateValue { applyDefaults = model.options.applyDefaults } encodedValue model.schema

                ( value, validationErrors ) =
                    case validationResult of
                        Ok validValue ->
                            ( validValue |> decodeValue JsonValue.decoder |> Result.withDefault NullValue, Dict.empty )

                        Err list ->
                            ( updatedValue, list |> dictFromListErrors )
            in
            ( { model
                | value = updatedValue
                , editingNow = str
                , validationErrors =
                    validationErrors

                --, edited = model.edited |> Dict.insert path True
              }
            , Cmd.none
            )
                => UpdateValue (value |> JsonValue.encode)

        StringInput path str ->
            let
                updatedValue =
                    model.value
                        |> JsonValue.setIn path (JsonValue.StringValue str)
                        |> Result.mapError (Debug.log "StringInput")
                        |> Result.withDefault model.value

                encodedValue =
                    updatedValue |> JsonValue.encode

                validationResult =
                    Json.Schema.validateValue { applyDefaults = model.options.applyDefaults } encodedValue model.schema

                ( value, validationErrors ) =
                    case validationResult of
                        Ok validValue ->
                            ( validValue |> decodeValue JsonValue.decoder |> Result.withDefault NullValue, Dict.empty )

                        Err list ->
                            ( updatedValue, list |> dictFromListErrors )
            in
            ( { model
                | value = updatedValue
                , editingNow = str
                , validationErrors =
                    validationErrors

                --, edited = model.edited |> Dict.insert path True
              }
            , Cmd.none
            )
                => UpdateValue (value |> JsonValue.encode)

        NumericInput path str ->
            let
                updatedValue =
                    str
                        |> String.toFloat
                        |> Result.andThen (\v -> JsonValue.setIn path (NumericValue v) model.value)
                        |> Result.mapError (Debug.log "NumericInput")
                        |> Result.withDefault model.value

                encodedValue =
                    updatedValue |> JsonValue.encode

                validationResult =
                    Json.Schema.validateValue { applyDefaults = model.options.applyDefaults } encodedValue model.schema

                ( value, validationErrors ) =
                    case validationResult of
                        Ok validValue ->
                            ( validValue |> decodeValue JsonValue.decoder |> Result.withDefault NullValue, Dict.empty )

                        Err list ->
                            ( updatedValue, list |> dictFromListErrors )
            in
            ( { model
                | value = value
                , editingNow = str
                , validationErrors =
                    validationErrors

                --, edited = model.edited |> Dict.insert path True
              }
            , Cmd.none
            )
                => UpdateValue (value |> JsonValue.encode)

        BoolInput path bool ->
            let
                updatedValue =
                    model.value
                        |> JsonValue.setIn path (BoolValue bool)
                        |> Result.mapError (Debug.log "BoolInput")
                        |> Result.withDefault model.value
            in
            ( { model | value = updatedValue }
            , Cmd.none
            )
                => UpdateValue (updatedValue |> JsonValue.encode)

        DeletePath path ->
            let
                value =
                    model.value
                        |> JsonValue.deleteIn path
                        |> Result.mapError (Debug.log "DeletePath")
                        |> Result.withDefault model.value
            in
            ( { model | value = value }
            , Cmd.none
            )
                => UpdateValue (value |> JsonValue.encode)

        AddItem path ->
            let
                nextIndex =
                    model.value
                        |> JsonValue.getIn path
                        |> Result.withDefault (ArrayValue [])
                        |> (\x ->
                                case x of
                                    ArrayValue l ->
                                        List.length l |> toString

                                    _ ->
                                        "0"
                           )

                itemPath =
                    path ++ [ nextIndex ]

                value =
                    model.value
                        |> JsonValue.setIn itemPath NullValue
                        |> Result.mapError (Debug.log "AddItem")
                        |> Result.withDefault model.value
            in
            ( { model | value = value }
            , makeId itemPath |> Dom.focus |> Task.attempt (\_ -> NoMsg)
            )
                => UpdateValue (value |> JsonValue.encode)

        AddProperty path ->
            let
                nextIndex =
                    model.value
                        |> JsonValue.getIn path
                        |> Result.withDefault (ArrayValue [])
                        |> (\x ->
                                case x of
                                    ObjectValue l ->
                                        List.length l

                                    _ ->
                                        0
                           )

                propPath =
                    path ++ [ "" ]

                value =
                    model.value
                        |> JsonValue.setIn propPath NullValue
                        |> Result.mapError (Debug.log "AddItem")
                        |> Result.withDefault model.value

                options =
                    model.options

                en =
                    path :: options.expandedNodes
            in
            ( { model
                | value = value
                , editPropPath = path
                , editPropIndex = Just nextIndex |> Debug.log "index"
                , editPropName = ""
                , options = { options | expandedNodes = en }
                , focusInput = []
              }
            , path
                |> String.join "/"
                |> (\x -> x ++ ":propname")
                |> Debug.log "will focus"
                |> Dom.focus
                |> Task.attempt
                    (\x ->
                        let
                            a =
                                Debug.log "focus" x
                        in
                        NoMsg
                    )
            )
                => SaveExpandedNodes en

        SetEditPropertyName propName path index ->
            ( { model
                | editPropPath = path
                , editPropIndex = Just index
                , editPropName = propName
                , focusInput = []
              }
            , path
                |> String.join "/"
                |> (\x -> x ++ ":propname")
                |> Dom.focus
                |> Task.attempt (\x -> NoMsg)
            )
                => NoOp

        EditPropertyName str ->
            ( { model | editPropName = str }
            , Cmd.none
            )
                => NoOp

        StopEditingPropertyName ->
            let
                updatedValue =
                    model.value
                        |> JsonValue.setPropertyName
                            ( model.editPropPath
                            , model.editPropIndex |> Maybe.withDefault 0
                            )
                            model.editPropName
                        |> Result.withDefault model.value
            in
            ( { model
                | editPropPath = []
                , editPropIndex = Nothing
                , value = updatedValue
              }
            , Cmd.none
            )
                => UpdateValue (updatedValue |> JsonValue.encode)

        ExpandNode path ->
            let
                options =
                    model.options

                en =
                    path :: options.expandedNodes
            in
            ( { model | options = { options | expandedNodes = en } }
            , Cmd.none
            )
                => SaveExpandedNodes en

        CollapseNode path ->
            let
                options =
                    model.options

                en =
                    options.expandedNodes
                        |> List.filter ((/=) path)
            in
            ( { model | options = { options | expandedNodes = en } }
            , Cmd.none
            )
                => SaveExpandedNodes en

        OpenMenu path ->
            ( { model | menu = Just path }
            , Cmd.none
            )
                => NoOp

        CloseMenu ->
            ( { model | menu = Nothing }
            , Cmd.none
            )
                => NoOp

        FocusInput path schema ->
            ( { model
                | focusInput = path
                , editingSchema = Just schema
                , editingNow =
                    case model.value |> getIn path of
                        Ok (StringValue s) ->
                            s

                        Ok (NumericValue s) ->
                            s |> toString

                        _ ->
                            ""
              }
            , Cmd.none
            )
                => NoOp

        BlurInput path ->
            if path == model.focusInput then
                ( { model
                    | focusInput = []
                    , editingSchema = Nothing
                    , edited = model.edited |> Dict.insert path True
                  }
                , Cmd.none
                )
                    => NoOp

            else
                ( model
                , Cmd.none
                )
                    => NoOp


view : Model -> View
view model =
    el None [ inlineStyle [ ( "font-family", "Menlo, monospace" ), ( "font-size", "12px" ), ( "line-height", "1.4" ) ], width <| percent 90 ] <|
        viewValue model model.schema model.value []


delete : Path -> View
delete path =
    Icons.xCircle
        |> Icons.withStrokeWidth 2
        |> Icons.withSize 18
        |> Icons.toHtml []
        |> Element.html
        |> el None
            [ onClick <| DeletePath path
            , width <| px 18
            , height <| px 18
            , class "action"
            , inlineStyle [ ( "cursor", "pointer" ) ]
            ]


isBlankSchema : Schema -> Bool
isBlankSchema =
    Schema.encode >> Encode.encode 0 >> (==) "{}"


pickOneOf : List Schema -> Value -> Schema
pickOneOf listSchemas value =
    let
        defaultResult =
            listSchemas
                |> List.head
                |> Maybe.withDefault blankSchema

        isValid s =
            Json.Schema.validateValue { applyDefaults = True } value s
                |> Result.toMaybe
                |> (/=) Nothing
    in
    listSchemas
        |> List.filter isValid
        |> List.head
        |> Maybe.withDefault defaultResult


resolve : Schema -> Schema -> Schema
resolve rootSchema rawSubSchema =
    let
        ( _, resolvedSchema ) =
            case rawSubSchema of
                ObjectSchema os ->
                    os.ref
                        |> Maybe.andThen (Ref.resolveReference "" Ref.defaultPool rootSchema)
                        |> Maybe.withDefault ( "", rawSubSchema )

                _ ->
                    ( "", rawSubSchema )
    in
    resolvedSchema


viewProperty : Model -> Bool -> Maybe Int -> Path -> String -> Schema -> JsonValue -> View
viewProperty model deletionAllowed indexInObject path key rawSubSchema value =
    let
        deeperLevelPath =
            path ++ [ key ]

        subSchema =
            case resolve model.schema rawSubSchema of
                ObjectSchema os ->
                    case os.anyOf of
                        Just schemas ->
                            value
                                |> JsonValue.encode
                                |> pickOneOf schemas
                                |> resolve model.schema

                        Nothing ->
                            ObjectSchema os

                x ->
                    x

        ( objectSchema, isArray, isDictionary ) =
            case subSchema of
                ObjectSchema os ->
                    ( Just os
                    , os.items /= NoItems
                    , case os.additionalProperties of
                        Just (BooleanSchema False) ->
                            False

                        _ ->
                            case value of
                                ObjectValue _ ->
                                    True

                                _ ->
                                    False
                    )

                _ ->
                    ( Nothing, False, False )

        isBlank =
            isBlankSchema subSchema

        isExpandable =
            if model.options.allowExpandingNodes then
                case value of
                    JsonValue.ObjectValue _ ->
                        isBlank |> not

                    JsonValue.ArrayValue _ ->
                        isBlank |> not

                    {-
                       isBlank
                           |> not
                           |> Debug.log (toString deeperLevelPath)
                    -}
                    _ ->
                        False

            else
                False

        isExpanded =
            if model.options.allowExpandingNodes then
                case value of
                    JsonValue.ObjectValue _ ->
                        isBlank
                            || List.member
                                deeperLevelPath
                                model.options.expandedNodes

                    JsonValue.ArrayValue _ ->
                        isBlank
                            || List.member
                                deeperLevelPath
                                model.options.expandedNodes

                    --List.member deeperLevelPath model.expandedNodes
                    _ ->
                        True

            else
                True

        propertyNamesAutocomplete =
            case subSchema of
                ObjectSchema os ->
                    case os.properties of
                        Just (Schemata list) ->
                            let
                                existingProps =
                                    case value of
                                        ObjectValue x ->
                                            x |> List.map (\( name, _ ) -> name)

                                        _ ->
                                            []
                            in
                            list
                                |> List.filterMap
                                    (\( propName, _ ) ->
                                        if List.member propName existingProps then
                                            Nothing

                                        else
                                            text propName
                                                |> Element.node "option"
                                                |> Just
                                    )
                                |> row None
                                    [ inlineStyle [ ( "display", "none" ) ]
                                    , Attributes.id
                                        (deeperLevelPath
                                            |> String.join "/"
                                            |> (\x -> x ++ ":props")
                                        )
                                    ]
                                |> Element.node "datalist"

                        Nothing ->
                            empty

                _ ->
                    empty
    in
    column None
        [ paddingTop 0 ]
        [ row None
            [ verticalCenter, spacing 5, class "key-container" ]
            [ if isExpandable then
                (if isExpanded then
                    Icons.chevronDown

                 else
                    Icons.chevronRight
                )
                    |> Icons.withSize 18
                    |> Icons.withStrokeWidth 2
                    |> Icons.toHtml []
                    |> Element.html
                    |> el None
                        [ width <| px 18
                        , height <| px 18
                        , inlineStyle [ ( "cursor", "pointer" ) ]
                        , if isExpanded then
                            onClick <| CollapseNode deeperLevelPath

                          else
                            onClick <| ExpandNode deeperLevelPath
                        ]

              else
                Icons.chevronDown
                    |> Icons.withSize 18
                    |> Icons.withStrokeWidth 2
                    |> Icons.toHtml []
                    |> Element.html
                    |> el None
                        [ width <| px 18
                        , height <| px 18
                        , inlineStyle [ ( "visibility", "hidden" ) ]
                        ]
            , if indexInObject /= Nothing && indexInObject == model.editPropIndex && path == model.editPropPath then
                row InputRow
                    [ vary Active True ]
                    [ model.editPropName
                        |> Element.inputText TextInput
                            [ onInput EditPropertyName
                            , onBlur <| StopEditingPropertyName
                            , path
                                |> String.join "/"
                                |> (\x -> x ++ ":propname")
                                |> Attributes.id
                            , path
                                |> String.join "/"
                                |> (\x -> x ++ ":props")
                                |> Attributes.list
                            ]
                    ]

              else
                (if model.options.useTitleAsLabel then
                    objectSchema
                        |> Maybe.map
                            (\os ->
                                case os.type_ of
                                    SingleType StringType ->
                                        empty

                                    SingleType IntegerType ->
                                        empty

                                    SingleType NumberType ->
                                        empty

                                    _ ->
                                        key |> text
                            )
                        |> Maybe.withDefault (key |> text)

                 else
                    key |> text
                )
                    |> el PropertyName
                        [ vary Active <| deeperLevelPath == model.focusInput
                        ]

            {-
               , objectSchema
                   |> Maybe.andThen .title
                   |> Maybe.withDefault key
                   |> text
            -}
            , Icons.moreVertical
                |> Icons.withSize 18
                |> Icons.withStrokeWidth 2
                |> Icons.toHtml []
                |> Element.html
                |> el None
                    [ class "action"
                    , inlineStyle [ ( "cursor", "pointer" ), ( "outline", "none" ) ]
                    , width <| px 18
                    , height <| px 18
                    , tabindex 2
                    , onFocus <| OpenMenu deeperLevelPath
                    , onBlur <| CloseMenu
                    ]
                |> Element.below
                    [ if Just deeperLevelPath == model.menu then
                        [ text "Edit as JSON" |> el MenuItem []
                        , case indexInObject of
                            Just index ->
                                text "Edit property name"
                                    |> el MenuItem
                                        [ onClick <| SetEditPropertyName key path index
                                        ]

                            _ ->
                                empty
                        , if isArray then
                            text "Add item" |> el MenuItem [ onClick <| AddItem deeperLevelPath ]

                          else if isDictionary then
                            text "Add property" |> el MenuItem [ onClick <| AddProperty deeperLevelPath ]

                          else
                            empty
                        ]
                            |> column None
                                [ inlineStyle
                                    [ ( "z-index", "2" )
                                    , ( "background", "white" )
                                    , ( "min-width", "200px" )
                                    , ( "border-radius", "2px" )
                                    , ( "box-shadow", "0 2px 2px 0 rgba(0,0,0,.14), 0 3px 1px -2px rgba(0,0,0,.2), 0 1px 5px 0 rgba(0,0,0,.12)" )
                                    ]
                                , padding 2
                                ]

                      else
                        empty
                    ]
            , if deletionAllowed then
                delete deeperLevelPath

              else
                empty
            ]

        --, displayDescription subSchema
        , if isExpanded then
            row None
                []
                [ viewValue model subSchema value deeperLevelPath
                , propertyNamesAutocomplete
                ]

          else
            empty
        ]


viewObject : Model -> Schema -> List ( String, JsonValue ) -> Bool -> Path -> List View
viewObject model schema props isArray path =
    let
        isOptional key required =
            case required of
                Just list ->
                    List.member key list |> not

                Nothing ->
                    True

        shouldRenderDefault required propName =
            if model.options.showEmptyOptionalProps then
                True

            else
                case required of
                    Just names ->
                        List.member propName names

                    Nothing ->
                        False

        iterateOverSchemata propsDict required (Schemata schemata) =
            schemata
                |> List.map
                    (\( propName, subSchema ) ->
                        case propsDict |> Dict.get propName of
                            Just value ->
                                viewProperty model (isOptional propName required && not model.options.showEmptyOptionalProps) Nothing path propName subSchema value

                            Nothing ->
                                if shouldRenderDefault required propName then
                                    viewProperty model (isOptional propName required && not model.options.showEmptyOptionalProps) Nothing path propName subSchema JsonValue.NullValue

                                else
                                    empty
                    )

        iterateOverProps isObject list schema =
            list
                |> List.indexedMap
                    (\index prop ->
                        case prop of
                            Just ( key, value ) ->
                                viewProperty model
                                    True
                                    (if isObject then
                                        Just index

                                     else
                                        Nothing
                                    )
                                    path
                                    key
                                    schema
                                    value

                            Nothing ->
                                empty
                    )
    in
    case schema of
        BooleanSchema True ->
            iterateOverProps True (props |> List.map Just) blankSchema

        BooleanSchema False ->
            iterateOverProps True (props |> List.map Just) disallowEverythingSchema

        ObjectSchema os ->
            let
                knownProperties =
                    case os.properties of
                        Just (Schemata x) ->
                            x
                                |> List.map (\( key, _ ) -> key)

                        _ ->
                            []

                justProps =
                    props
                        |> List.map Just

                extraProps =
                    props
                        |> List.map
                            (\( name, v ) ->
                                if List.member name knownProperties then
                                    Nothing

                                else
                                    Just ( name, v )
                            )
            in
            if isBlankSchema schema then
                [ ObjectValue props
                    |> JsonValue.encode
                    |> Encode.encode 4
                    |> text
                    |> el SourceCode [ paddingLeft 10 ]
                ]

            else
                if isArray then
                    case os.items of
                        NoItems ->
                            iterateOverProps False justProps blankSchema

                        ItemDefinition s ->
                            iterateOverProps False justProps s

                        -- TODO: hande arrayOfItems
                        _ ->
                            iterateOverProps False justProps disallowEverythingSchema

                else
                    [ os.properties
                        |> Maybe.map (iterateOverSchemata (Dict.fromList props) os.required)
                        |> Maybe.withDefault []
                    , case os.additionalProperties of
                        Just (ObjectSchema os) ->
                            iterateOverProps True extraProps (ObjectSchema os)

                        Just (BooleanSchema False) ->
                            iterateOverProps True extraProps disallowEverythingSchema

                        _ ->
                            iterateOverProps True extraProps blankSchema
                    ]
                        |> List.concat


disallowEverythingSchema : Schema
disallowEverythingSchema =
    ObjectSchema { blankSubSchema | not = Just blankSchema }


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


makeId : Path -> String
makeId path =
    String.join "/" path ++ ":value"


viewNumber : Model -> Schema -> Maybe Float -> Path -> View
viewNumber model schema numValue path =
    let
        isFocused =
            path == model.focusInput
    in
    row
        InputRow
        [ vary Active isFocused ]
        [ (if isFocused then
            model.editingNow

           else
            numValue |> Maybe.map toString |> Maybe.withDefault ""
          )
            |> Element.inputText TextInput
                [ onInput <| NumericInput path
                , onFocus <| FocusInput path schema
                , onBlur <| BlurInput path
                , Attributes.type_ "number"
                , width <| fill 1
                , Attributes.id <| makeId path
                ]
        ]


viewBool : Model -> Schema -> Bool -> Path -> View
viewBool model schema boolValue path =
    let
        ( icon, label, color ) =
            if boolValue then
                ( Icons.toggleRight, "true", "darkgreen" )

            else
                ( Icons.toggleLeft, "false", "darkred" )
    in
    row None
        [ onClick <| BoolInput path <| not boolValue
        , verticalCenter
        , spacing 5
        , inlineStyle [ ( "cursor", "pointer" ), ( "color", color ) ]
        ]
        [ icon
            |> Icons.withSize 18
            |> Icons.toHtml []
            |> Element.html
            |> el None
                [ width <| px 18, height <| px 18 ]
        , label |> text
        ]


labeledInput : Model -> (Path -> String -> Msg) -> Schema -> String -> Path -> View
labeledInput model inputHandler schema stringValue path =
    let
        listId =
            String.join "/" path ++ ":list"

        isFocused =
            path == model.focusInput

        objectSchema =
            case schema of
                ObjectSchema os ->
                    Just os

                _ ->
                    Nothing

        autocompleteOptions =
            objectSchema
                |> Maybe.andThen .enum
                |> Maybe.map
                    (\enum ->
                        enum
                            |> List.map
                                (\v ->
                                    let
                                        strValue =
                                            v |> decodeValue Decode.string |> Result.withDefault ""
                                    in
                                    Element.node "option" <| text strValue
                                )
                            |> row None [ inlineStyle [ ( "display", "none" ) ], Attributes.id listId ]
                            |> Element.node "datalist"
                    )
                |> Maybe.withDefault empty

        inputId =
            makeId path

        hasValue =
            (if isFocused then
                model.editingNow

             else
                stringValue
            )
                /= ""

        hasError =
            model.validationErrors
                |> Dict.get path
                |> Maybe.withDefault []
                |> List.isEmpty
                |> not

        highlightAsError =
            hasError && (model.options.showInitialValidationErrors || (model.edited |> Dict.member path))
    in
    if isBlankSchema schema then
        row
            InputRow
            [ vary Active isFocused ]
            [ stringValue
                |> toString
                |> Element.textArea TextInput
                    [ onInput <| ValueInput path
                    , onFocus <| FocusInput path schema
                    , onBlur <| BlurInput path
                    , width <| fill 1
                    , Attributes.id <| makeId path
                    ]
            ]

    else
        column
            InputRow
            [ vary Active isFocused
            , paddingLeft 16
            , paddingRight 16
            , inlineStyle [ ( "background", "transparent" ), ( "height", "56px" ), ( "position", "relative" ) ]
            ]
            [ (if isFocused then
                model.editingNow

               else
                stringValue
              )
                |> Element.inputText TextInput
                    [ onFocus <| FocusInput path schema
                    , onBlur <| BlurInput path
                    , onInput <| inputHandler path
                    , Attributes.list listId
                    , Attributes.id inputId
                    , Attributes.autocomplete False
                    , inlineStyle
                        [ ( "position", "absolute" )
                        , ( "bottom", "8px" )
                        , ( "left", "16px" )
                        , ( "right", "16px" )
                        , ( "width", "calc(100% - 32px)" )
                        , ( "top", "auto" )
                        ]
                    ]
            , autocompleteOptions
            , (if model.options.useTitleAsLabel then
                objectSchema
                    |> Maybe.andThen .title
                    |> (\x ->
                            case x of
                                Just t ->
                                    Just t

                                Nothing ->
                                    path |> List.reverse |> List.head
                       )
                    |> Maybe.map text
                    |> Maybe.withDefault empty

               else
                empty
              )
                |> el None
                    [ inlineStyle
                        [ ( "transform-origin", "left top" )
                        , if isFocused || hasValue then
                            ( "transform", "translateY(-100%) scale(0.75, 0.75)" )

                          else
                            ( "cursor", "text" )
                        , ( "transform-origin", "left top" )
                        , ( "left", "16px" )
                        , ( "position", "absolute" )
                        , ( "bottom", "8px" )
                        , ( "top", "auto" )
                        , ( "font-size", "14px" )
                        , ( "transition", "transform 180ms cubic-bezier(0.4, 0, 0.2, 1)" )
                        , ( "color"
                          , if highlightAsError then
                                "red"

                            else
                                "black"
                          )
                        ]
                    , Attributes.for inputId
                    ]
                |> Element.node "label"
            ]


viewValue : Model -> Schema -> JsonValue -> Path -> View
viewValue model schema value path =
    let
        isFocused =
            model.focusInput == path
    in
    (case value of
        JsonValue.ObjectValue ov ->
            viewObject model schema ov False path

        JsonValue.ArrayValue av ->
            viewObject model schema (av |> List.indexedMap (\index val -> ( toString index, val ))) True path

        JsonValue.StringValue sv ->
            [ labeledInput model StringInput schema sv path ]

        JsonValue.NumericValue nv ->
            [ labeledInput model NumericInput schema (toString nv) path ]

        JsonValue.BoolValue bv ->
            [ viewBool model schema bv path ]

        JsonValue.NullValue ->
            case schema of
                ObjectSchema os ->
                    case os.type_ of
                        SingleType StringType ->
                            [ labeledInput model StringInput schema "" path ]

                        SingleType IntegerType ->
                            [ viewNumber model schema Nothing path ]

                        _ ->
                            [ row InputRow
                                [ vary Active <| isFocused ]
                                [ "null"
                                    |> Element.textArea TextInput
                                        [ onInput <| ValueInput path
                                        , width <| fill 1
                                        , onFocus <| FocusInput path schema
                                        , onBlur <| BlurInput path
                                        ]
                                ]
                            ]

                _ ->
                    [ row InputRow
                        [ vary Active <| model.focusInput == path ]
                        [ "null"
                            |> Element.textArea TextInput
                                [ onInput <| ValueInput path
                                , width <| fill 1
                                , onFocus <| FocusInput path schema
                                , onBlur <| BlurInput path
                                ]
                        ]
                    ]
    )
        |> column None [ spacing 0, width <| fill 1 ]
        |> Element.below
            [ case model.validationErrors |> Dict.get path of
                Just errors ->
                    if model.options.showInitialValidationErrors || (model.edited |> Dict.member path) then
                        errors
                            |> List.filter ((/=) "")
                            |> List.map ((++) "Error: " >> text >> el InlineError [])
                            |> column None [ paddingLeft 16, paddingTop 8 ]

                    else
                        empty

                Nothing ->
                    case schema of
                        ObjectSchema os ->
                            case os.description of
                                Just d ->
                                    if isFocused then
                                        text d |> el None [ paddingLeft 16, paddingTop 8, inlineStyle [ ( "font-size", "10px" ) ] ]

                                    else
                                        empty

                                Nothing ->
                                    empty

                        _ ->
                            empty
            ]


(=>) : a -> b -> ( a, b )
(=>) a b =
    ( a, b )
