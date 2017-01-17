
module Main exposing (main)

import Animation
import Animation.Architecture exposing (..)
import EveryDict exposing (..)
import Html exposing (..)


-- MODEL


type ElementId
  -- TODO: Replace this definition
  = Stub_ElementId


type AnimationId
  -- TODO: Replace this definition
  = Stub_AnimationId


type alias Model =
  { animationModel : AnimationModel ElementId AnimationId Msg
  -- TODO: Extend this definition
  }


setAnimationModel : AnimationModel ElementId AnimationId Msg -> Model -> Model
setAnimationModel animationModel model =
  { model | animationModel = animationModel }


-- UPDATE


type Msg
  = AnimationCompleted  ElementId AnimationId   -- Signal completion of an animation
  | ExecuteAnimation    ElementId AnimationId   -- Start a new animation
  | UpdateAnimation     Animation.Msg           -- Update an "in progress" animation
  -- TODO: Extend this definition


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    
    AnimationCompleted elementId animationId ->
      ( model, Cmd.none )

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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Animation.subscription UpdateAnimation <| values model.animationModel.animations


-- VIEW


view : Model -> Html Msg
view model =
  -- TODO: Replace this definition
  div [] []


-- ENTRY POINT


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }


-- Initialize and fire off any "at startup" animations
init : (Model, Cmd Msg)
init =
  -- TODO: Replace this definition
  ( Model (AnimationModel empty animationDictMap)
  , Cmd.none
  )


-- Define function mappings for different hashed animations
animationDictMap : AnimationDictMap AnimationId msg
animationDictMap =
  -- TODO: Replace this definition
  fromList []
