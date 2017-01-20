
module Showcase.List exposing (..)

import Animation
import Animation.Messenger
import Animation.Architecture exposing (..)
import EveryDict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Task exposing (..)
import Update.Extra exposing (..)

import Showcase.Animations exposing (..)


-- MODEL


type ElementId
  = Container
  | RowElement Int


type alias Model =
  { animationModel : AnimationModel ElementId AnimationId Msg
  -- Extended this definition beyond the template from here
  , containerHasAppeared : Bool
  }


setAnimationModel : AnimationModel ElementId AnimationId Msg -> Model -> Model
setAnimationModel animationModel model =
  { model | animationModel = animationModel }


setContainerHasAppeared : Bool -> Model -> Model
setContainerHasAppeared hasAppeared model =
  { model | containerHasAppeared = hasAppeared }


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

    -- TODO: Extend this definition


performNextCommand : Model -> ElementId -> AnimationId -> ( Model, Cmd Msg )
performNextCommand model elementId animationId =
  case elementId of
    
    Container ->
      case animationId of
        
        FadeInAndSlideUp ->
          ( model
              |> setContainerHasAppeared True
          , performUpdates [ (ExecuteAnimation (RowElement 1) FullWidth) ]
          )
        
        _ ->
          ( model, Cmd.none )
    
    RowElement n ->
      if not model.containerHasAppeared
        then ( model, Cmd.none )
        else
          if n < 5
            then ( model, performUpdates [ (ExecuteAnimation (RowElement (n + 1)) FullWidth) ] )
            else ( model, Cmd.none )


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
    (  (renderAnimationsByKey Container model.animationModel)
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
        , "background-color"  => "#fcfcff"
        , "border"            => "1px solid #333399"
        , "display"           => "inline-block"
        ]
      ]
      [ renderElement model 1
      , renderElement model 2
      , renderElement model 3
      , renderElement model 4
      , renderElement model 5
      ]
    ]


renderElement : Model -> Int -> Html Msg
renderElement model rowNum =
  div
    (  (renderAnimationsByKey (RowElement rowNum) model.animationModel)
    ++ [ style
         [ "padding"           => "10px 0 10px 0"
         , "background-color"  => "#f6f6ff"
         , "border-bottom"     => "1px solid #333399"
         , "white-space"       => "nowrap"
         , "overflow"          => "hidden"
         ]
       ]
    )
    [ text <| "RowElement " ++ toString rowNum ]


initializeModel : Model
initializeModel =
  let
    dict =
      animationDictMap
        |> addCustomAnimation "RowElementDefault" (setToDefault defaultRowElementAnimation)
  in
    Model (AnimationModel empty dict) False


initializeCmdMsg : Cmd Msg
initializeCmdMsg =
  Cmd.batch
    ( List.map
        (Task.perform (\(elementId, animationId) -> ExecuteAnimation elementId animationId))
        -- Elm processes batched commands starting from the tail of the list!
        [ Task.succeed (Container, FadeInAndSlideUp)
        , Task.succeed (RowElement 1, Custom "RowElementDefault")
        , Task.succeed (RowElement 2, Custom "RowElementDefault")
        , Task.succeed (RowElement 3, Custom "RowElementDefault")
        , Task.succeed (RowElement 4, Custom "RowElementDefault")
        , Task.succeed (RowElement 5, Custom "RowElementDefault")
        ]
    )


init : (Model, Cmd Msg)
init =
  ( initializeModel, initializeCmdMsg )


defaultRowElementAnimation : Animation.Messenger.State msg
defaultRowElementAnimation =
  Animation.style
    [ Animation.width (Animation.percent 0)
    ]
