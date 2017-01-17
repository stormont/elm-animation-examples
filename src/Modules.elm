
module Main exposing (main)

import Animation
import Animation.Architecture exposing (..)
import EveryDict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task exposing (..)
import Update.Extra exposing (..)

import AppAnimations exposing (..)


-- MODEL


type ElementId
  = Element01
  | Element02


type alias Model =
  { animationModel    : AnimationModel ElementId AnimationId Msg
  , viewDisplayStyles : EveryDict ElementId String
  }


setAnimationModel : AnimationModel ElementId AnimationId Msg -> Model -> Model
setAnimationModel animationModel model =
  { model | animationModel = animationModel }


setViewDisplayStyles : EveryDict ElementId String -> Model -> Model
setViewDisplayStyles viewDisplayStyles model =
  { model | viewDisplayStyles = viewDisplayStyles }


-- UPDATE


type Msg
  = AnimationCompleted  ElementId AnimationId   -- Signal completion of an animation
  | ExecuteAnimation    ElementId AnimationId   -- Start a new animation
  | SwapElements        ElementId ElementId     -- Swap visibility of two elements
  | UpdateAnimation     Animation.Msg           -- Update an "in progress" animation


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    
    AnimationCompleted elementId animationId ->
      ( model
          |> setViewDisplayStyles (insert elementId (setDisplayStyle animationId) model.viewDisplayStyles)
      , Cmd.none
      )

    ExecuteAnimation elementId animationId ->
      let
        completedAnimMsg = AnimationCompleted elementId animationId
        anims = executeAnimation elementId animationId completedAnimMsg model.animationModel
      in
        ( model
            |> setAnimationModel (setAnimations anims model.animationModel)
            |> setViewDisplayStyles (insert elementId "block" model.viewDisplayStyles)
        , Cmd.none
        )

    SwapElements srcView destView ->
      performUpdates
        update model
        [ (ExecuteAnimation srcView  FadeOut)
        , (ExecuteAnimation destView FadeIn)
        ]
    
    UpdateAnimation animMsg ->
      let
        (cmds, anims) = updateAnimations animMsg model.animationModel.animations
      in
        ( model
            |> setAnimationModel (setAnimations anims model.animationModel)
        , Cmd.batch cmds
        )


setDisplayStyle : AnimationId -> String
setDisplayStyle animationId =
  if animationId == FadeOut || animationId == Opac0
    then "none"
    else "block"


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Animation.subscription UpdateAnimation <| values model.animationModel.animations


-- VIEW


view : Model -> Html Msg
view model =
  div
    []
    [ element01View model
    , element02View model
    ]


element01View : Model -> Html Msg
element01View model =
  let
    displayStyle =
      case get Element01 model.viewDisplayStyles of
        Nothing -> "block"
        Just x  -> x
  in
    div
      (  (renderAnimationsByKey Element01 model.animationModel)
      ++ [ onClick (SwapElements Element01 Element02)
        , style
            [ ( "position", "fixed" )
            , ( "margin", "100px auto" )
            , ( "padding", "25px" )
            , ( "width", "200px" )
            , ( "height", "50px" )
            , ( "background-color", "#268bd2" )
            , ( "color", "white" )
            , ( "left", "40%" )
            , ( "display", displayStyle )
            ]
          ]
      )
      [ text "Click to Animate Element01!" ]


element02View : Model -> Html Msg
element02View model =
  let
    displayStyle =
      case get Element02 model.viewDisplayStyles of
        Nothing -> "block"
        Just x  -> x
  in
    div
      (  (renderAnimationsByKey Element02 model.animationModel)
      ++ [ onClick (SwapElements Element02 Element01)
        , style
            [ ( "position", "fixed" )
            , ( "margin", "250px auto" )
            , ( "padding", "25px" )
            , ( "width", "200px" )
            , ( "height", "50px" )
            , ( "background-color", "#8bd226" )
            , ( "color", "white" )
            , ( "left", "40%" )
            , ( "display", displayStyle )
            ]
          ]
      )
      [ text "Click to Animate Element02!" ]


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
  ( Model (AnimationModel empty animationDictMap) empty
  , Cmd.batch
      ( List.map
          (Task.perform (\(elementId, animationId) -> ExecuteAnimation elementId animationId))
          [ Task.succeed (Element01, FadeIn)
          , Task.succeed (Element02, Opac0)
          ]
      )
  )
