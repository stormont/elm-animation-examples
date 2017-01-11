
module Main exposing (main)

import Animation
import Animation.Messenger
import Dict exposing (..)
import Ease exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task exposing (..)


-- ENTRY POINT


main : Program Never Model Msg
main =
  Html.program
    { init = initializeApp
    , subscriptions = subscriptions
    , update = update
    , view = view
    }


-- MODEL


type alias ElementId =
  String


type alias AnimationId =
  String


type alias AnimationTuple =
  ( List (Animation.Messenger.Step Msg) -> Animation.Messenger.State Msg -> Animation.Messenger.State Msg
  , List (Animation.Messenger.Step Msg)
  , Animation.Messenger.State Msg
  )


type alias AnimationMappings =
  Dict AnimationId AnimationTuple


type alias Model =
  { animations        : Dict ElementId (Animation.Messenger.State Msg)
  , animationMappings : AnimationMappings
  , viewDisplayStyles : Dict ElementId String
  }


initializeModel : Model
initializeModel =
  Model empty animationMappings empty


-- Return the animation, or if the animation hasn't been defined yet, use the provided default
animationOrDefault
  :  Maybe (Animation.Messenger.State Msg)
  -> Animation.Messenger.State Msg
  -> Animation.Messenger.State Msg
animationOrDefault manim def =
  case manim of
    Nothing -> def
    Just a  -> a


-- UPDATE


type Msg
  = Initialize
  -- Signal completion of an animation
  | AnimationCompleted ElementId AnimationId
  -- Start a new animation
  | ExecuteAnimation ElementId AnimationId
  -- Update an "in progress" animation
  | UpdateAnimation Animation.Msg
  -- Swap visibility of two elements
  | SwapElements ElementId ElementId


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Initialize ->
      ( model, Cmd.none )
    
    AnimationCompleted elementId animationId ->
      let
        _ = Debug.log (toString msg) ()
        displayStyle =
          if animationId == "fade_out" || animationId == "opac_0"
            then "none"
            else "block"
        viewDisplayStyles = insert elementId displayStyle model.viewDisplayStyles
      in
        ( { model | viewDisplayStyles = viewDisplayStyles }
        , Cmd.none
        )

    ExecuteAnimation elementId animationId ->
      let
        _ = Debug.log (toString msg) ()
        executeAnimation dict =
          case get animationId model.animationMappings of
            Nothing -> dict
            Just (f, xs, def) ->
              let
                startAnim = animationOrDefault (get elementId dict) def
                xs_ = xs ++ [ Animation.Messenger.send (AnimationCompleted elementId animationId) ]
              in
                insert elementId (f xs_ startAnim ) dict
        viewDisplayStyles = insert elementId "block" model.viewDisplayStyles
      in
        ( { model | animations = executeAnimation model.animations, viewDisplayStyles = viewDisplayStyles }
        , Cmd.none
        )
    
    UpdateAnimation animMsg ->
      let
        f k v         =  Animation.Messenger.update animMsg v
                      |> (\(v, c) -> (c, (k, v)))
        (cmds, anims) =  toList model.animations
                      |> List.map (\(k, v) -> f k v)
                      |> List.unzip
      in
        ( { model | animations = fromList anims }
        , Cmd.batch cmds
        )

    SwapElements srcView destView ->
      performChainedUpdates
        update
        model
        [ (ExecuteAnimation srcView  "fade_out")
        , (ExecuteAnimation destView "fade_in")
        ]


type alias Update =
  (Msg -> Model -> (Model, Cmd Msg))


chainUpdate : Update -> Msg -> (Model, Cmd Msg) -> (Model, Cmd Msg)
chainUpdate updateFunc msg (model, cmds) =
  let
    (model_, cmds_) = updateFunc msg model
  in
    model_ ! [ cmds, cmds_ ]


performChainedUpdates : Update -> Model -> List Msg -> (Model, Cmd Msg)
performChainedUpdates updateFunc model cmds =
  List.foldl (chainUpdate updateFunc) (model ! []) cmds


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Animation.subscription UpdateAnimation <| values model.animations


-- VIEW


view : Model -> Html Msg
view =
  appView


renderAnimationsByElementId : ElementId -> Model -> List (Attribute Msg)
renderAnimationsByElementId elementId model =
  toList model.animations
    |> List.filter (\(k, _) -> k == elementId)
    |> List.map (\(_, v) -> Animation.render v)
    |> List.concat


{-
-- EVERYTHING ABOVE HERE IS COOKIE-CUTTER STUFF THAT CAN BE COPY-PASTED AS A STARTING POINT FOR ANY APPLICATION
-- ***
-- EVERYTHING BELOW HERE IS CUSTOM STUFF FOR YOUR SPECIFIC APPLICATION
-}


appView : Model -> Html Msg
appView model =
  div
    []
    [ element01View model
    , element02View model
    ]


element01View : Model -> Html Msg
element01View model =
  let
    displayStyle =
      case get "Element01" model.viewDisplayStyles of
        Nothing -> "block"
        Just x  -> x
  in
    div
      (  (renderAnimationsByElementId "Element01" model)
      ++ [ onClick (SwapElements "Element01" "Element02")
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
      case get "Element02" model.viewDisplayStyles of
        Nothing -> "block"
        Just x  -> x
  in
    div
      (  (renderAnimationsByElementId "Element02" model)
      ++ [ onClick (SwapElements "Element02" "Element01")
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


-- Initialize and fire off any "at startup" animations
initializeApp : (Model, Cmd Msg)
initializeApp =
  ( initializeModel
  , Cmd.batch
      ( List.map
          (Task.perform (\(elementId, animationId) -> ExecuteAnimation elementId animationId))
          [ Task.succeed ("Element01", "fade_in")
          , Task.succeed ("Element02", "opac_0")
          ]
      )
  )


-- Define function mappings for our different hashed animations
animationMappings : AnimationMappings
animationMappings =
  fromList
    [ ( "fade_in",          fadeIn )
    , ( "fade_out",         fadeOut )
    , ( "fade_out_fade_in", fadeOutFadeIn )
    , ( "opac_0",           opac_0 )
    , ( "opac_1",           opac_1 )
    ]


-- Definitions for specific animations


-- Example of basic animation usage
fadeIn : AnimationTuple
fadeIn =
  ( -- Style of animation execution
    Animation.interrupt
    -- Animation to perform
  , [ Animation.to [ Animation.opacity 1 ] ]
    -- Default starting animation state, if none exists
  , Animation.style [ Animation.opacity 0 ]
  )


-- Example of basic animation usage
fadeOut : AnimationTuple
fadeOut =
  ( -- Style of animation execution
    Animation.interrupt
    -- Animation to perform
  , [ Animation.to [ Animation.opacity 0 ] ]
    -- Default starting animation state, if none exists
  , Animation.style [ Animation.opacity 1 ]
  )


-- Example of animation with custom duration and easing
fadeOutFadeIn : AnimationTuple
fadeOutFadeIn =
  ( -- Style of animation execution
    Animation.interrupt
    -- Animation to perform
  , [ Animation.toWithEach [ (Animation.easing { duration = 500, ease = linear },  Animation.opacity 0) ]
    , Animation.toWithEach [ (Animation.easing { duration = 300, ease = outExpo }, Animation.opacity 1) ]
    ]
    -- Default starting animation state, if none exists
  , Animation.style [ Animation.opacity 1 ]
  )


-- Example of basic animation usage
opac_0 : AnimationTuple
opac_0 =
  ( -- Style of animation execution
    Animation.interrupt
    -- Animation to perform
  , [ Animation.to [ Animation.opacity 0 ] ]
    -- Default starting animation state, if none exists
  , Animation.style [ Animation.opacity 0 ]
  )


-- Example of basic animation usage
opac_1 : AnimationTuple
opac_1 =
  ( -- Style of animation execution
    Animation.interrupt
    -- Animation to perform
  , [ Animation.to [ Animation.opacity 1 ] ]
    -- Default starting animation state, if none exists
  , Animation.style [ Animation.opacity 1 ]
  )
