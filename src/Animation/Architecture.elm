
module Animation.Architecture exposing
  ( AnimationDictMap
  , AnimationModel
  , AnimationTuple
  , concat
  , concat_
  , executeAnimation
  , renderAnimationsByKey
  , setAnimations
  , updateAnimations
  )

import Animation
import Animation.Messenger
import EveryDict exposing (..)
import Html exposing (..)
import List.Extra exposing (foldl1)


type alias AnimationDictMap key msg =
  EveryDict key (AnimationTuple msg)


type alias ActiveAnimations key msg =
  EveryDict key (Animation.Messenger.State msg)


type alias AnimationState model key msg =
  { model | animations : ActiveAnimations key msg }


type alias AnimationExecutionFunction msg =
  List (Animation.Messenger.Step msg)
    -> Animation.Messenger.State msg
    -> Animation.Messenger.State msg


type alias AnimationTuple msg =
  ( AnimationExecutionFunction msg        -- Animation execution function (e.g., interrupt, queue, etc.)
  , List (Animation.Messenger.Step msg)   -- Animation steps to perform
  , Animation.Messenger.State msg         -- Default starting animation state, if none exists
  )


type alias AnimationModel key1 key2 msg =
  { animations       : ActiveAnimations key1 msg  -- Animations in progress
  , animationDictMap : AnimationDictMap key2 msg  -- Collection of defined animations
  }


-- Return the animation, or if the animation hasn't been defined yet, use the provided default
animationOrDefault
  :  Maybe (Animation.Messenger.State msg)
  -> Animation.Messenger.State msg
  -> Animation.Messenger.State msg
animationOrDefault manim def =
  case manim of
    Nothing -> def
    Just a  -> a


-- Takes the animation execution and default animation of the first tuple
-- and concatenates all of the lists of animation steps
concat
    :  List  (AnimationTuple msg)
    -> Maybe (AnimationTuple msg)
concat =
  foldl1 (\(f, xs, d) (_, ys ,_) -> (f, xs ++ ys, d))


concat_
    :  List (AnimationTuple msg)
    -> AnimationTuple msg
concat_ anims =
  case foldl1 (\(f, xs, d) (_, ys ,_) -> (f, xs ++ ys, d)) anims of
    Nothing -> (Animation.interrupt, [], Animation.style [])  -- Just an empty/no-op animation
    Just anim -> anim


extend
    :  List (Animation.Messenger.Step msg)
    -> AnimationTuple msg
    -> AnimationTuple msg
extend steps =
  (\(f, xs, def) -> (f, xs ++ steps, def))


executeAnimation
    : key1
    -> key2
    -> msg
    -> AnimationModel key1 key2 msg
    -> ActiveAnimations key1 msg
executeAnimation active_key lookup_key msg model =
  let
    dict = model.animations
  in
    case get lookup_key model.animationDictMap of
      Nothing -> dict
      Just (f, xs, def) ->
        let
          startAnim = animationOrDefault (get active_key dict) def
          xs_ = xs ++ [ Animation.Messenger.send msg ]
        in
          insert active_key (f xs_ startAnim ) dict


renderAnimationsByKey
    : key
    -> AnimationState model key msg
    -> List (Attribute msg)
renderAnimationsByKey key model =
  toList model.animations
    |> List.filter (\(k, _) -> k == key)
    |> List.map (\(_, v) -> Animation.render v)
    |> List.concat


setAnimations
    : ActiveAnimations key1 msg
    -> AnimationModel key1 key2 msg
    -> AnimationModel key1 key2 msg
setAnimations anims model =
  { model | animations = anims }


updateAnimations
    : Animation.Msg
    -> ActiveAnimations key msg
    -> ( List (Cmd msg), ActiveAnimations key msg )
updateAnimations animMsg animations =
  let
    f k v =  Animation.Messenger.update animMsg v
          |> (\(v, c) -> (c, (k, v)))
  in
    toList animations
      |> List.map (\(k, v) -> f k v)
      |> List.unzip
      |> (\(c, a) -> (c, fromList a))
