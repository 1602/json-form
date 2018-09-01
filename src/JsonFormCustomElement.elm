module JsonFormCustomElement exposing (main)

import Browser
import CustomElement exposing (init, subscriptions, update, view)
import Html


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
