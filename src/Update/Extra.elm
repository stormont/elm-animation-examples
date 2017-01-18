
module Update.Extra exposing (..)

import Task


type alias UpdateFunc model msg =
  ( msg -> model -> (model, Cmd msg) )


chainUpdate
    : UpdateFunc model msg
    -> msg
    -> ( model, Cmd msg )
    -> ( model, Cmd msg )
chainUpdate updateFunc msg (model, cmds) =
  let
    (model_, cmds_) = updateFunc msg model
  in
    model_ ! [ cmds, cmds_ ]


performUpdates_
    : UpdateFunc model msg
    -> model
    -> List msg
    -> ( model, Cmd msg )
performUpdates_ updateFunc model cmds =
  List.foldl (chainUpdate updateFunc) (model ! []) cmds


performUpdates : List msg -> Cmd msg
performUpdates messages =
  List.map (\x -> Task.perform (\_ -> x) (Task.succeed ())) messages
    |> Cmd.batch
