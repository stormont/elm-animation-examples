
module Main exposing (main)

import Html exposing (..)
import Update.Extra exposing (..)

import Showcase.Animations exposing (..)
import Showcase.List
import Showcase.Login


-- MODEL


type ActiveModel
  = LoginModel
  | ListModel


type alias Model =
  { activeModel : ActiveModel
  , loginModel : Showcase.Login.Model
  , listModel : Showcase.List.Model
  }


setActiveModel : ActiveModel -> Model -> Model
setActiveModel activeModel model =
  { model | activeModel = activeModel }


setLoginModel : Showcase.Login.Model -> Model -> Model
setLoginModel loginModel model =
  { model | loginModel = loginModel }


setListModel : Showcase.List.Model -> Model -> Model
setListModel listModel model =
  { model | listModel = listModel }


initializeModel : Model
initializeModel =
  Model
    LoginModel
    Showcase.Login.initializeModel
    Showcase.List.initializeModel


-- UPDATE


type Msg
  = LoginMsg Showcase.Login.Msg
  | ListMsg Showcase.List.Msg
  | SetActiveModel ActiveModel


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    LoginMsg (Showcase.Login.AnimationCompleted Showcase.Login.Form FadeOutAndSlideLeft) ->
      let
        (m, c) = Showcase.Login.update
                  (Showcase.Login.AnimationCompleted Showcase.Login.Form FadeOutAndSlideLeft)
                  model.loginModel
      in
        ( model
            |> setLoginModel m
        , Cmd.batch
            [ performUpdates [ SetActiveModel ListModel ]
            , Cmd.map LoginMsg c
            ]
        )
    
    LoginMsg message ->
      let
        (m, c) = Showcase.Login.update message model.loginModel
      in
        ( model
            |> setLoginModel m
        , Cmd.map LoginMsg c
        )

    ListMsg message ->
      let
        (m, c) = Showcase.List.update message model.listModel
      in
        ( model
            |> setListModel m
        , Cmd.map ListMsg c
        )

    SetActiveModel m ->
      let
        cmd =
          case m of
            LoginModel -> Cmd.map LoginMsg Showcase.Login.initializeCmdMsg
            ListModel  -> Cmd.map ListMsg  Showcase.List.initializeCmdMsg
      in
        ( model
            |> setActiveModel m
        , cmd
        )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map LoginMsg <| Showcase.Login.subscriptions model.loginModel
    , Sub.map ListMsg  <| Showcase.List.subscriptions  model.listModel
    ]


-- VIEW


view : Model -> Html Msg
view model =
  case model.activeModel of
    LoginModel -> Showcase.Login.view model.loginModel |> Html.map LoginMsg
    ListModel  -> Showcase.List.view  model.listModel  |> Html.map ListMsg


-- ENTRY POINT


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }


init : ( Model, Cmd Msg )
init =
  ( initializeModel, Cmd.map LoginMsg Showcase.Login.initializeCmdMsg )
