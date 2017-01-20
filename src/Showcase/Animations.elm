
module Showcase.Animations exposing
  ( AnimationId(..)
  , addCustomAnimation
  , animationDictMap
  , setToDefault
  )

import Animation exposing (..)
import Animation.Messenger
import Animation.Architecture exposing (..)
import Ease exposing (..)
import EveryDict exposing (..)
import Time


easeWith : (Float -> Float) -> Time.Time -> Animation.Interpolation
easeWith ease duration =
  easing { duration = duration, ease = ease }


mapToWithEach
    : Animation.Interpolation
    -> List Animation.Property
    -> Animation.Messenger.Step msg
mapToWithEach func xs =
  List.map (\x -> (func, x)) xs
    |> toWithEach


type AnimationId
  = FadeIn
  | SlideUp
  | FadeInAndSlideUp
  | FadeOutAndSlideLeft
  | Transparent
  | FullyOpaque
  | ZeroWidth
  | FullWidth
  | Custom String   -- For those cases where you want to define your own custom animations as extensions


animationDictMap : AnimationDictMap AnimationId msg
animationDictMap =
  fromList
    [ ( FadeIn,  fadeIn )
    , ( SlideUp, slideUp )
    , ( FadeInAndSlideUp, fadeInAndSlideUp )
    , ( FadeOutAndSlideLeft, fadeOutAndSlideLeft )
    , ( Transparent, opacity_0 )
    , ( FullyOpaque, opacity_1 )
    , ( ZeroWidth, width_0 )
    , ( FullWidth, width_1 )
    ]


addCustomAnimation
    : String
    -> AnimationTuple msg
    -> AnimationDictMap AnimationId msg
    -> AnimationDictMap AnimationId msg
addCustomAnimation customId animation dict =
  toList dict
    |> (++) [ (Custom customId, animation) ]
    |> fromList


setToDefault : Animation.Messenger.State msg -> AnimationTuple msg
setToDefault def =
  ( Animation.interrupt
  , []
  , def
  )


fadeIn : AnimationTuple msg
fadeIn =
  ( interrupt
  , [ toWithEach [ (easeWith inExpo 500, opacity 1) ] ]
  , style [ opacity 0 ]
  )


slideUp : AnimationTuple msg
slideUp =
  ( interrupt
  , [ toWithEach [ (easeWith outSine 500, top (percent 10)) ] ]
  , style [ top (percent 75) ]
  )


fadeInAndSlideUp : AnimationTuple msg
fadeInAndSlideUp =
  ( interrupt
  , [ toWithEach
      [ (easeWith inExpo 500, opacity 1)
      , (easeWith outSine 500, top (percent 10))
      ]
    ]
  , style
    [ opacity 0
    , top (percent 75)
    ]
  )


fadeOutAndSlideLeft : AnimationTuple msg
fadeOutAndSlideLeft =
  ( interrupt
  , [ toWithEach
      [ (easeWith outExpo 500, opacity 0)
      , (easeWith inSine 500, right (percent 50))
      ]
    ]
  , style
    [ opacity 1
    , right (percent 0)
    ]
  )


opacity_0 : AnimationTuple msg
opacity_0 =
  ( interrupt
  , [ to [ opacity 0 ] ]
  , style [ opacity 1 ]
  )


opacity_1 : AnimationTuple msg
opacity_1 =
  ( interrupt
  , [ to [ opacity 1 ] ]
  , style [ opacity 0 ]
  )


width_0 : AnimationTuple msg
width_0 =
  ( interrupt
  , [ toWithEach [ (easeWith outExpo 500, width (percent 0)) ] ]
  , style [ width (percent 100) ]
  )


width_1 : AnimationTuple msg
width_1 =
  ( interrupt
  , [ toWithEach [ (easeWith outExpo 500, width (percent 100)) ] ]
  , style [ width (percent 0) ]
  )
