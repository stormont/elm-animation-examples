
module Showcase.Animations exposing
  ( AnimationId(..)
  , animationDictMap
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


animationDictMap : AnimationDictMap AnimationId msg
animationDictMap =
  fromList
    [ ( FadeIn,  fadeIn )
    , ( SlideUp, slideUp )
    , ( FadeInAndSlideUp, fadeInAndSlideUp )
    , ( FadeOutAndSlideLeft, fadeOutAndSlideLeft )
    , ( Transparent, opacity_0 )
    , ( FullyOpaque, opacity_1 )
    ]


fadeIn : AnimationTuple msg
fadeIn =
  ( interrupt
  , [ toWithEach [ (easeWith inExpo 500, opacity 1) ] ]
  , defaultAnimation
  )


slideUp : AnimationTuple msg
slideUp =
  ( interrupt
  , [ toWithEach [ (easeWith outSine 500, top (percent 10)) ] ]
  , defaultAnimation
  )


fadeInAndSlideUp : AnimationTuple msg
fadeInAndSlideUp =
  ( interrupt
  , [ toWithEach
      [ (easeWith inExpo 500, opacity 1)
      , (easeWith outSine 500, top (percent 10))
      ]
    ]
  , defaultAnimation
  )


fadeOutAndSlideLeft : AnimationTuple msg
fadeOutAndSlideLeft =
  ( interrupt
  , [ toWithEach
      [ (easeWith outExpo 500, opacity 0)
      , (easeWith inSine 500, right (percent 50))
      ]
    ]
  , defaultAnimation
  )


-- Example of basic animation usage
opacity_0 : AnimationTuple msg
opacity_0 =
  ( interrupt
  , [ to [ opacity 0 ] ]
  , defaultAnimation
  )


-- Example of basic animation usage
opacity_1 : AnimationTuple msg
opacity_1 =
  ( interrupt
  , [ to [ opacity 1 ] ]
  , defaultAnimation
  )


defaultAnimation : Animation.Messenger.State msg
defaultAnimation =
  style
    [ opacity 0
    , right (percent 0)
    , top (percent 75)
    ]
