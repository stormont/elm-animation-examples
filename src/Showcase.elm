
module Main exposing (main)

import Html exposing (..)

import Showcase.Login


-- ENTRY POINT


main : Program Never Showcase.Login.Model Showcase.Login.Msg
main =
  Html.program
    { init = Showcase.Login.init
    , subscriptions = Showcase.Login.subscriptions
    , update = Showcase.Login.update
    , view = Showcase.Login.view
    }
