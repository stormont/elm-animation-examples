
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


type alias AnimationMappings =
  Dict AnimationId (Maybe (Animation.Messenger.State Msg) -> Animation.Messenger.State Msg)


type alias Model =
  { animations        : Dict ElementId (Animation.Messenger.State Msg)
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


type alias Update =
  (Msg -> Model -> (Model, Cmd Msg))


type Msg
  = Initialize
  -- Start a new animation
  | ExecuteAnimation ElementId AnimationId
  -- Update an "in progress" animation
  | UpdateAnimation Animation.Msg
  | SwapElements ElementId ElementId


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Initialize ->
      ( model, Cmd.none )

    ExecuteAnimation elementId animationId ->
      let
        _ = Debug.log ("ExecuteAnimation " ++ elementId ++ " " ++ animationId) ()
        executeAnimation dict =
          case get animationId model.animationMappings of
            Just f  -> insert elementId (f <| get elementId dict) dict
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

    SwapElements srcView destView ->
      performChainedUpdates
        update
        model
        [ (ExecuteAnimation srcView  "fade_out")
        , (ExecuteAnimation destView "fade_in")
        ]
      {- LOOKS LIKE THIS WHEN UNROLLED
      let
        (m1, c1) = update (ExecuteAnimation srcView  "fade_out") model
        (m2, c2) = update (ExecuteAnimation destView "fade_in")  m1
      in
        ( m2, Cmd.batch [ c1, c2 ] )
      -}


chainUpdate : Update -> Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
chainUpdate updateFunc msg (model, cmds) =
  let
    (model_, cmds_) = updateFunc msg model
  in
    model_ ! [ cmds, cmds_ ]


performChainedUpdates : Update -> Model -> List Msg -> ( Model, Cmd Msg )
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
  div
    (  (renderAnimationsByElementId "Element01" model)
    ++ [ onClick (SwapElements "Element01" "Element02")
       , style
          [ ( "position", "relative" )
          , ( "margin", "100px auto" )
          , ( "padding", "25px" )
          , ( "width", "200px" )
          , ( "height", "50px" )
          , ( "background-color", "#268bd2" )
          , ( "color", "white" )
          ]
        ]
    )
    [ text "Click to Animate Element01!" ]


element02View : Model -> Html Msg
element02View model =
  div
    (  (renderAnimationsByElementId "Element02" model)
    ++ [ onClick (SwapElements "Element02" "Element01")
       , style
          [ ( "position", "relative" )
          , ( "margin", "100px auto" )
          , ( "padding", "25px" )
          , ( "width", "200px" )
          , ( "height", "50px" )
          , ( "background-color", "#8bd226" )
          , ( "color", "white" )
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
fadeIn : Maybe (Animation.Messenger.State Msg) -> Animation.Messenger.State Msg
fadeIn manim =
  Animation.interrupt
    -- Animation to perform
    [ Animation.to [ Animation.opacity 1 ] ]
    -- Prior animation state, or a default
    (animationOrDefault manim (Animation.style [ Animation.opacity 0 ]))


-- Example of basic animation usage
fadeOut : Maybe (Animation.Messenger.State Msg) -> Animation.Messenger.State Msg
fadeOut manim =
  Animation.interrupt
    -- Animation to perform
    [ Animation.to [ Animation.opacity 0 ] ]
    -- Prior animation state, or a default
    (animationOrDefault manim (Animation.style [ Animation.opacity 1 ]))


-- Example of animation with custom duration and easing
fadeOutFadeIn : Maybe (Animation.Messenger.State Msg) -> Animation.Messenger.State Msg
fadeOutFadeIn manim =
  Animation.interrupt
    -- Animation to perform
    [ Animation.toWithEach [ (Animation.easing { duration = 500, ease = linear },  Animation.opacity 0) ]
    , Animation.toWithEach [ (Animation.easing { duration = 300, ease = outExpo }, Animation.opacity 1) ]
    ]
    -- Prior animation state, or a default
    (animationOrDefault manim (Animation.style [ Animation.opacity 1 ]))


-- Example of basic animation usage
opac_0 : Maybe (Animation.Messenger.State Msg) -> Animation.Messenger.State Msg
opac_0 manim =
  Animation.interrupt
    -- Animation to perform
    [ Animation.to [ Animation.opacity 0 ] ]
    -- Prior animation state, or a default
    (animationOrDefault manim (Animation.style [ Animation.opacity 0 ]))


-- Example of basic animation usage
opac_1 : Maybe (Animation.Messenger.State Msg) -> Animation.Messenger.State Msg
opac_1 manim =
  Animation.interrupt
    -- Animation to perform
    [ Animation.to [ Animation.opacity 1 ] ]
    -- Prior animation state, or a default
    (animationOrDefault manim (Animation.style [ Animation.opacity 1 ]))
