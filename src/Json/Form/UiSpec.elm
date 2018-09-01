module Json.Form.UiSpec exposing (UiSpec(..), decoder)

import Json.Decode as Decode exposing (Decoder, Value, fail, succeed)


type UiSpec
    = PasswordField
    | Switch
    | Unknown Value


decoder : Decoder UiSpec
decoder =
    Decode.oneOf
        [ Decode.field "widget" Decode.string
            |> Decode.andThen
                (\widget ->
                    if widget == "password" then
                        succeed PasswordField

                    else if widget == "switch" then
                        succeed Switch

                    else
                        fail ""
                )
        , Decode.value |> Decode.map Unknown
        ]
