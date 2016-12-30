
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


type alias AnimationId =
  String


type alias AnimationMappings =
  Dict AnimationId (Maybe (Animation.Messenger.State Msg) -> Animation.Messenger.State Msg)


type alias Model =
  { animations        : Dict AnimationId (Animation.Messenger.State Msg)
  , animationMappings : AnimationMappings
  }


initializeModel : Model
initializeModel =
  Model empty animationMappings


-- Return the animation, or if the animation hasn't been defined yet, use the provided default
animationOrDefault : Maybe (Animation.Messenger.State Msg) -> Animation.Messenger.State Msg -> Animation.Messenger.State Msg
animationOrDefault manim def =
  case manim of
    Nothing -> def
    Just a  -> a


-- UPDATE


type Msg
  = Initialize
  -- Start a new animation
  | ExecuteAnimation AnimationId
  -- Update an "in progress" animation
  | UpdateAnimation Animation.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Initialize ->
      ( model, Cmd.none )

    ExecuteAnimation animationId ->
      let
        _ = Debug.log ("ExecuteAnimation " ++ animationId) ()
        executeAnimation dict =
          case get animationId model.animationMappings of
            Just f  -> insert animationId (f <| get animationId dict) dict
            Nothing -> dict
      in
        ( { model | animations = executeAnimation model.animations }
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


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Animation.subscription UpdateAnimation <| values model.animations


-- VIEW


view : Model -> Html Msg
view =
  appView


{-
-- EVERYTHING ABOVE HERE IS COOKIE-CUTTER STUFF THAT CAN BE COPY-PASTED AS A STARTING POINT FOR ANY APPLICATION
-- ***
-- EVERYTHING BELOW HERE IS CUSTOM STUFF FOR YOUR SPECIFIC APPLICATION
-}


appView : Model -> Html Msg
appView model =
  div
    (  (values model.animations |> List.map Animation.render |> List.concat)
    ++ [ onClick (ExecuteAnimation "fade_out_fade_in")
       , style
          [ ( "position", "relative" )
          , ( "margin", "100px auto" )
          , ( "padding", "25px" )
          , ( "width", "200px" )
          , ( "height", "200px" )
          , ( "background-color", "#268bd2" )
          , ( "color", "white" )
          ]
        ]
    )
    [ text "Click to Animate!" ]


-- Initialize and fire off any "at startup" animations
initializeApp : (Model, Cmd Msg)
initializeApp =
  ( initializeModel
  , Task.perform (\identifier -> ExecuteAnimation identifier) (Task.succeed "fade_in")
  )


-- Define function mappings for our different hashed animations
animationMappings : AnimationMappings
animationMappings =
  fromList
    [ ( "fade_in",          fadeIn )
    , ( "fade_out_fade_in", fadeOutFadeIn )
    ]


-- Definitions for specific animations


-- Example of basic animation usage
fadeIn : Maybe (Animation.Messenger.State Msg) -> Animation.Messenger.State Msg
fadeIn manim =
  Animation.interrupt
    -- Animation to perform
    [ Animation.to [ Animation.opacity 1 ] ]
    -- Prior animation state, or a default
    (animationOrDefault manim (Animation.style [ Animation.opacity 0.0 ]))


-- Example of animation with custom duration and easing
fadeOutFadeIn : Maybe (Animation.Messenger.State Msg) -> Animation.Messenger.State Msg
fadeOutFadeIn manim =
  Animation.interrupt
    -- Animation to perform
    [ Animation.toWithEach [ (Animation.easing { duration = 500, ease = linear },  Animation.opacity 0) ]
    , Animation.toWithEach [ (Animation.easing { duration = 300, ease = outExpo }, Animation.opacity 1) ]
    ]
    -- Prior animation state, or a default
    (animationOrDefault manim (Animation.style [ Animation.opacity 1.0 ]))
