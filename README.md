# elm-animation-examples
This repo contains examples of HTML animations using [Elm 0.18](http://elm-lang.org/) and the [`mdgriffith/elm-style-animation`](http://package.elm-lang.org/packages/mdgriffith/elm-style-animation/3.5.1) package.

## Basic Animation Framework

The [`BasicFramework.elm`](https://github.com/stormont/elm-animation-examples/blob/master/src/BasicFramework.elm) module demonstrates a basic extension of the standard [Elm Architecture](https://guide.elm-lang.org/architecture/) to layer multiple effects upon the same element.

## Swapping Views

The [`SwapView.elm`](https://github.com/stormont/elm-animation-examples/blob/master/src/SwapView.elm) module starts from the Basic Animation Framework and uses it to animate swapping visibility between two different view elements.

## Listening to Completed Animations

The [`FinishAnimation.elm`](https://github.com/stormont/elm-animation-examples/blob/master/src/FinishAnimation.elm) module extends the _SwapView_ example to add the capability to send notifications at the end of an animation.

## Modularize!

The [`Modules.elm`](https://github.com/stormont/elm-animation-examples/blob/master/src/Modules.elm) example takes what's been done in the previous parts and splits the functionality across different modules:
* **Modules.elm:** The main entry point/custom app example.
* **Animation/Architecture.elm:** The foundational, reusable pieces of the architecture.
* **AppAnimations.elm:** The custom animations for the app (that would be reused across different view components).
* **Update/Extra.elm:** The small extension functions for use with the `update` function, refactored. These may be bundled as a separate package in the future.

While `Modules.elm` is a complete example, some of its parts provide a similar-but-incomplete basis as the basic Elm Architecture. [`MinimalModules.elm`](https://github.com/stormont/elm-animation-examples/blob/master/src/MinimalModules.elm) provides a "minimal" version of the modularized code to use as a starting template for future work.
