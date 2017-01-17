
module AppAnimations exposing (..)

import Animation
import Animation.Architecture exposing (..)
import Ease exposing (..)
import EveryDict exposing (..)


type AnimationId
  = FadeIn
  | FadeOut
  | FadeOutFadeIn
  | Opac0
  | Opac1


-- Define function mappings for different hashed animations
animationDictMap : AnimationDictMap AnimationId msg
animationDictMap =
  fromList
    [ ( FadeIn,        fadeIn )
    , ( FadeOut,       fadeOut )
    , ( FadeOutFadeIn, fadeOutFadeIn )
    , ( Opac0,         opac_0 )
    , ( Opac1,         opac_1 )
    ]


-- Example of basic animation usage
fadeIn : AnimationTuple msg
fadeIn =
  ( Animation.interrupt
  , [ Animation.to [ Animation.opacity 1 ] ]
  , Animation.style [ Animation.opacity 0 ]
  )


-- Example of basic animation usage
fadeOut : AnimationTuple msg
fadeOut =
  ( Animation.interrupt
  , [ Animation.to [ Animation.opacity 0 ] ]
  , Animation.style [ Animation.opacity 1 ]
  )


-- Example of animation with custom duration and easing
fadeOutFadeIn : AnimationTuple msg
fadeOutFadeIn =
  ( Animation.interrupt
  , [ Animation.toWithEach [ (Animation.easing { duration = 500, ease = linear },  Animation.opacity 0) ]
    , Animation.toWithEach [ (Animation.easing { duration = 300, ease = outExpo }, Animation.opacity 1) ]
    ]
  , Animation.style [ Animation.opacity 1 ]
  )


-- Example of basic animation usage
opac_0 : AnimationTuple msg
opac_0 =
  ( Animation.interrupt
  , [ Animation.to [ Animation.opacity 0 ] ]
  , Animation.style [ Animation.opacity 0 ]
  )


-- Example of basic animation usage
opac_1 : AnimationTuple msg
opac_1 =
  ( Animation.interrupt
  , [ Animation.to [ Animation.opacity 1 ] ]
  , Animation.style [ Animation.opacity 1 ]
  )
