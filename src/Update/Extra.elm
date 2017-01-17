
module Update.Extra exposing (..)


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


performUpdates
    : UpdateFunc model msg
    -> model
    -> List msg
    -> ( model, Cmd msg )
performUpdates updateFunc model cmds =
  List.foldl (chainUpdate updateFunc) (model ! []) cmds
