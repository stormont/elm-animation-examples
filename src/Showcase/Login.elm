
module Showcase.Login exposing (..)

import Animation
import Animation.Messenger
import Animation.Architecture exposing (..)
import Dom exposing (..)
import EveryDict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task exposing (..)
import Update.Extra exposing (..)

import Showcase.Animations exposing (..)


-- MODEL


type ElementId
  = Form
  | Inputs
  | UsernameField
  | PasswordField
  | SubmitButton


type alias Model =
  { animationModel : AnimationModel ElementId AnimationId Msg
  -- Extended this definition beyond the template from here
  , fieldsDisabled : Bool
  , usernameText   : String
  , passwordText   : String
  }


setAnimationModel : AnimationModel ElementId AnimationId Msg -> Model -> Model
setAnimationModel animationModel model =
  { model | animationModel = animationModel }


setFieldsDisabled : Bool -> Model -> Model
setFieldsDisabled disabled model =
  { model | fieldsDisabled = disabled }


setUsernameText : String -> Model -> Model
setUsernameText text model =
  { model | usernameText = text }


setPasswordText : String -> Model -> Model
setPasswordText text model =
  { model | passwordText = text }


submitDisabled : Model -> Bool
submitDisabled model =
  model.usernameText == "" || model.passwordText == ""


-- UPDATE


type Msg
  = AnimationCompleted  ElementId AnimationId   -- Signal completion of an animation
  | ExecuteAnimation    ElementId AnimationId   -- Start a new animation
  | UpdateAnimation     Animation.Msg           -- Update an "in progress" animation
  -- Extended this definition beyond the template from here
  | FormReady
  | InputChanged        ElementId String
  | SetInitialFocus     (Result Error ())
  | SubmitForm


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    
    AnimationCompleted elementId animationId ->
      -- Extended this definition beyond the template in a separate function call
      performNextCommand model elementId animationId

    ExecuteAnimation elementId animationId ->
      let
        completedAnimMsg = AnimationCompleted elementId animationId
        anims = executeAnimation elementId animationId completedAnimMsg model.animationModel
      in
        ( model
            |> setAnimationModel (setAnimations anims model.animationModel)
        , Cmd.none
        )
    
    UpdateAnimation animMsg ->
      let
        (cmds, anims) = updateAnimations animMsg model.animationModel.animations
      in
        ( model
            |> setAnimationModel (setAnimations anims model.animationModel)
        , Cmd.batch cmds
        )

    -- Extended this definition beyond the template from here

    FormReady ->
      ( model
          |> setFieldsDisabled False
      , setFocus model UsernameField
      )
    
    InputChanged elementId text ->
      let
        model_ =
          case elementId of
            UsernameField -> model |> setUsernameText text
            PasswordField -> model |> setPasswordText text
            _ -> model
      in
        if submitDisabled model_
          then ( model_, performUpdates [ (ExecuteAnimation SubmitButton Transparent) ] )
          else ( model_, performUpdates [ (ExecuteAnimation SubmitButton FullyOpaque) ] )

    SetInitialFocus _ ->
      ( model, Cmd.none )

    SubmitForm ->
      ( model, performUpdates [ (ExecuteAnimation Form FadeOutAndSlideLeft) ] )


performNextCommand : Model -> ElementId -> AnimationId -> ( Model, Cmd Msg )
performNextCommand model elementId animationId =
  case elementId of
    Form ->
      case animationId of
        FadeInAndSlideUp ->
          ( model, performUpdates [ (ExecuteAnimation Inputs FullyOpaque) ] )
        _ ->
          ( model, Cmd.none )
    Inputs ->
      case animationId of
        FullyOpaque ->
          ( model, performUpdates [ FormReady ] )
        _ ->
          ( model, Cmd.none )
    _ ->
      ( model, Cmd.none )


setFocus : Model -> ElementId -> Cmd Msg
setFocus model elementId =
  toString elementId
    |> focus
    |> Task.attempt SetInitialFocus


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Animation.subscription UpdateAnimation <| values model.animationModel.animations


-- VIEW


(=>) : a -> b -> ( a, b )
(=>) = (,)


view : Model -> Html Msg
view =
  renderViewContainer


renderViewContainer : Model -> Html Msg
renderViewContainer model =
  div
    (  (renderAnimationsByKey Form model.animationModel)
    ++ [ style
          [ "text-align"  => "center"
          , "position"    => "fixed"
          , "width"       => "100%"
          ]
       ]
    )
    [ div
      [ style
        [ "width"             => "300px"
        , "height"            => "300px"
        , "background-color"  => "#f6f6ff"
        , "border"            => "1px solid #333399"
        , "display"           => "inline-block"
        ]
      ]
      [ renderViewInputs model ]
    ]


renderViewInputs : Model -> Html Msg
renderViewInputs model =
  div
    ( (renderAnimationsByKey Inputs model.animationModel) )
    [ h4 [] [ text "(Just enter something in both fields)" ]
    , input
        [ style
          [ "width"     => "260px"
          , "padding"   => "10px"
          , "font-size" => "18px"
          ]
        , placeholder "Enter username"
        , id <| toString UsernameField
        , disabled model.fieldsDisabled
        , onInput (\x -> InputChanged UsernameField x)
        ] []
    , p [] []
    , input
        [ style
          [ "width"     => "260px"
          , "padding"   => "10px"
          , "font-size" => "18px"
          ]
        , placeholder "Enter password"
        , type_ "password"
        , id <| toString PasswordField
        , disabled model.fieldsDisabled
        , onInput (\x -> InputChanged PasswordField x)
        ] []
    , p [] []
    , button
        (  (renderAnimationsByKey SubmitButton model.animationModel)
        ++ [ style
              [ "width"     => "280px"
              , "padding"   => "10px"
              , "font-size" => "18px"
              ]
           , disabled (submitDisabled model)
           , onClick SubmitForm
           ]
        )
        [ text "Submit" ]
    ]


initializeModel : Model
initializeModel =
  let
    dict =
      animationDictMap
        |> addCustomAnimation "Default" (setToDefault defaultAnimation)
        |> addCustomAnimation "DefaultForm" (setToDefault defaultFormAnimation)
  in
    Model (AnimationModel empty dict) True "" ""


initializeCmdMsg : Cmd Msg
initializeCmdMsg =
  Cmd.batch
    ( List.map
        (Task.perform (\(elementId, animationId) -> ExecuteAnimation elementId animationId))
        [ Task.succeed (Form, FadeInAndSlideUp)
        , Task.succeed (Form, Custom "DefaultForm")
        , Task.succeed (Inputs, Custom "Default")
        , Task.succeed (SubmitButton, Custom "Default")
        ]
    )


init : (Model, Cmd Msg)
init =
  ( initializeModel, initializeCmdMsg )


defaultAnimation : Animation.Messenger.State msg
defaultAnimation =
  Animation.style
    [ Animation.opacity 0
    , Animation.right (Animation.percent 0)
    , Animation.top (Animation.percent 75)
    ]


defaultFormAnimation : Animation.Messenger.State msg
defaultFormAnimation =
  Animation.style
    [ Animation.opacity 0
    , Animation.right (Animation.percent 0)
    , Animation.top (Animation.percent 75)
    ]
