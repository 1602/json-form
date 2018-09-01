module Json.Form.Definitions exposing (EditingMode(..), Model, Msg(..), Path, init)

import Dict exposing (Dict)
import Json.Schema.Definitions exposing (..)
import Json.Value exposing (JsonValue)


type alias Model =
    { schema : Schema
    , focused : Maybe Path
    , value : Maybe JsonValue
    , errors : Dict Path (List String)
    , beingEdited : List Path
    , editedNumber : String
    }


init : Schema -> Maybe JsonValue -> Model
init s v =
    { schema = s
    , focused = Nothing
    , value = v
    , errors = Dict.empty
    , beingEdited = []
    , editedNumber = ""
    }


type Msg
    = FocusInput (Maybe Path)
    | FocusNumericInput (Maybe Path)
    | EditValue Path JsonValue
    | EditNumber String


type EditingMode
    = TextField
    | NumberField
    | Switch
    | Checkbox
    | JsonEditor
    | Object


type alias Path =
    List String
