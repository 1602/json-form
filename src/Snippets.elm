module Snippets exposing (Snippet(..), getSnippet, getSnippetTitle, index)

import Json.Encode as Encode exposing (list, object, string)
import Json.Schema.Builder exposing (..)
import Json.Schema.Definitions exposing (Schema(..), SingleType(..), Type(..), blankSchema, blankSubSchema)


type Snippet
    = SimpleField
    | FlatObject
    | LoginForm
    | Rules
    | FlightBooking


index : List Snippet
index =
    [ SimpleField
    , FlatObject
    , LoginForm
    , Rules
    , FlightBooking
    ]


getSnippetTitle : Snippet -> String
getSnippetTitle ds =
    case ds of
        SimpleField ->
            "Simple Text Field"

        FlatObject ->
            "Flat Object"

        LoginForm ->
            "Login Form"

        Rules ->
            "Rules"

        FlightBooking ->
            "Flight Booking"


getSnippet : Snippet -> Schema
getSnippet ds =
    case ds of
        SimpleField ->
            ObjectSchema
                { blankSubSchema
                    | type_ = SingleType StringType
                    , title = Just "First name"
                    , description = Just "First (given) name of a travelling person"
                }

        FlatObject ->
            buildSchema
                |> withType "object"
                |> withProperties
                    [ ( "firstName"
                      , buildSchema
                            |> withType "string"
                            |> withTitle "First name"
                            |> withDescription "First (given) name of a travelling person"
                            |> withMinLength 2
                      )
                    , ( "middleName"
                      , buildSchema
                            |> withType "string"
                            |> withTitle "Middle name"
                      )
                    , ( "lastName"
                      , buildSchema
                            |> withType "string"
                            |> withTitle "Last name"
                            |> withDescription "Last (family) name of a travelling person"
                      )
                    ]
                |> withAdditionalProperties (boolSchema False)
                |> toSchema
                |> Result.withDefault blankSchema

        LoginForm ->
            buildSchema
                |> withType "object"
                |> withProperties
                    [ ( "email"
                      , buildSchema
                            |> withType "string"
                            |> withTitle "Email"
                            |> withMinLength 2
                      )
                    , ( "password"
                      , buildSchema
                            |> withType "string"
                            |> withTitle "Password"
                            |> withDescription "Must contain enough various symbols"
                            |> withCustomKeyword "ui" (Encode.object [ ( "widget", string "password" ) ])
                      )
                    , ( "remember"
                      , buildSchema
                            |> withType "boolean"
                            |> withTitle "remember me"
                            |> withDescription "This will keep you logged in for another 2 weeks"
                            |> withCustomKeyword "ui" (Encode.object [ ( "widget", string "checkbox" ) ])
                      )
                    ]
                |> withAdditionalProperties (boolSchema False)
                |> toSchema
                |> Result.withDefault blankSchema

        Rules ->
            let
                withRule name =
                    withCustomKeyword "ui"
                        (object
                            [ ( "rule"
                              , object
                                    [ ( "action", string name )
                                    , ( "path", list [ string "enabled" ] )
                                    , ( "condition"
                                      , object
                                            [ ( "const", Encode.bool False )
                                            , ( "default", Encode.bool False )
                                            ]
                                      )
                                    ]
                              )
                            ]
                        )

                withRuleAndWidget name widget =
                    withCustomKeyword "ui"
                        (object
                            [ ( "rule"
                              , object
                                    [ ( "action", string name )
                                    , ( "path", list [ string "enabled" ] )
                                    , ( "condition"
                                      , object
                                            [ ( "const", Encode.bool False )
                                            , ( "default", Encode.bool False )
                                            ]
                                      )
                                    ]
                              )
                            , ( "widget", string widget )
                            ]
                        )
            in
            buildSchema
                |> withType "object"
                |> withProperties
                    [ ( "enabled"
                      , buildSchema
                            |> withType "boolean"
                            -- |> withDefault (Encode.bool True)
                            |> withTitle "enable"
                            |> withDescription "Enable editing"
                            |> withCustomKeyword "ui" (Encode.object [ ( "widget", string "switch" ) ])
                      )
                    , ( "form"
                      , buildSchema
                            |> withType "object"
                            |> withProperties
                                [ ( "disableDemo"
                                  , buildSchema
                                        |> withTitle "Rule: disable text field"
                                        |> withType "string"
                                        |> withDescription "This field will be enabled when switch turned on"
                                        |> withRule "disable"
                                  )
                                , ( "hideDemo"
                                  , buildSchema
                                        |> withTitle "Rule: hide text field"
                                        |> withType "string"
                                        |> withDescription "This field will be shown when switch turned on"
                                        |> withRule "hide"
                                  )
                                , ( "disableNumericDemo"
                                  , buildSchema
                                        |> withTitle "Rule: disable numeric"
                                        |> withType "number"
                                        |> withDescription "This numeric field will be enabled when switch turned on"
                                        |> withRule "disable"
                                  )
                                , ( "hideNumericDemo"
                                  , buildSchema
                                        |> withTitle "Rule: hide numeric"
                                        |> withType "number"
                                        |> withDescription "This numeric field will be shown when switch turned on"
                                        |> withRule "hide"
                                  )
                                , ( "disableCheckboxDemo"
                                  , buildSchema
                                        |> withTitle "Rule: disable checkbox"
                                        |> withType "boolean"
                                        |> withDescription "This checkbox will be enabled when switch turned on"
                                        |> withRuleAndWidget "disable" "checkbox"
                                  )
                                , ( "disableSwitchDemo"
                                  , buildSchema
                                        |> withTitle "Rule: disable switch"
                                        |> withType "boolean"
                                        |> withDescription "This switch will be enabled when switch turned on"
                                        |> withRuleAndWidget "disable" "switch"
                                  )
                                , ( "nestedForm"
                                  , buildSchema
                                        |> withType "object"
                                        |> withProperties
                                            [ ( "disableDemo"
                                              , buildSchema
                                                    |> withTitle "Rule: disable nested form"
                                                    |> withType "string"
                                                    |> withDescription "This field will be enabled when switch turned on"
                                              )
                                            ]
                                        |> withRule "disable"
                                  )
                                , ( "otherNestedForm"
                                  , buildSchema
                                        |> withType "object"
                                        |> withProperties
                                            [ ( "hideDemo"
                                              , buildSchema
                                                    |> withTitle "Rule: hide nested form"
                                                    |> withType "string"
                                                    |> withDescription "This field will be hidden when switch turned on"
                                              )
                                            ]
                                        |> withRule "hide"
                                  )
                                ]
                      )
                    ]
                |> toSchema
                |> Result.withDefault blankSchema

        FlightBooking ->
            buildSchema
                |> withType "object"
                |> withRequired [ "airlineCode" ]
                |> withProperties
                    [ ( "pan"
                      , buildSchema
                            |> withType "string"
                            |> withTitle "Card Number"
                            |> withDescription "A card number. When not provided, this will be generated in the background"
                            |> withMinLength 10
                            |> withMaxLength 17
                      )
                    , ( "airlineCode"
                      , buildSchema
                            |> withType "string"
                            |> withTitle "Airline Code"
                            |> withDescription "Two letter IATA airline code"
                            |> withMinLength 2
                            |> withMaxLength 2
                      )
                    , ( "return"
                      , buildSchema
                            |> withType "boolean"
                            |> withTitle "Return"
                            |> withDescription "One way or return flight"
                            |> withCustomKeyword "ui" (Encode.object [ ( "widget", string "switch" ) ])
                      )
                    , ( "flightType"
                      , buildSchema
                            |> withType "string"
                            |> withTitle "Flight Type"
                            |> withDescription "Flight type is one of any/international/domestic"
                            |> withEnum
                                [ string "domestic"
                                , string "international"
                                , string "any"
                                ]
                            |> withDefault (string "any")
                      )
                    , ( "cabinClass"
                      , buildSchema
                            |> withType "string"
                            |> withTitle "Cabin Class"
                            |> withDescription "Specify a cabin class. One of economy/premium economy/business/first"
                            |> withEnum
                                [ string "economy"
                                , string "premium economy"
                                , string "business"
                                , string "first"
                                ]
                      )
                    , ( "earliestOffset"
                      , buildSchema
                            |> withType "number"
                            |> withTitle "Earliest Offset"
                            |> withDescription "Minutes before the first outbound flight may be chosen from the time of endpoint call"
                            |> withMinimum 0
                      )
                    , ( "latestOffset"
                      , buildSchema
                            |> withType "number"
                            |> withTitle "Latest Offset"
                            |> withDescription "Minutes after the earliest offset that the last outbound flight may be chosen"
                            |> withMinimum 0
                      )
                    , ( "minPassengers"
                      , buildSchema
                            |> withType "number"
                            |> withTitle "Minimum Passengers"
                            |> withDescription "No less than specified number of passengers will be generated"
                            |> withMinimum 0
                      )
                    , ( "maxPassengers"
                      , buildSchema
                            |> withType "number"
                            |> withTitle "Maximum Passengers"
                            |> withDescription "No more than specified number of passengers will be generated"
                            |> withMinimum 0
                      )
                    ]
                |> withAdditionalProperties (boolSchema False)
                |> toSchema
                |> Result.withDefault blankSchema
