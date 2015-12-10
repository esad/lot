module App where

import Keyboard
import Signal exposing (Signal, Address)
import Signal.Extra exposing ((~>), keepWhen)
import Model exposing (Action(..))
import Char
import View
import Html
import Task

modelIsEditing : Signal.Mailbox Bool
modelIsEditing =
  Signal.mailbox False

port updateModelIsEditing : Signal (Task.Task x ())
port updateModelIsEditing =
  (model ~> Model.isEditing |> Signal.dropRepeats) ~> (Signal.send modelIsEditing.address)

port focus : Signal Bool
port focus =
  modelIsEditing.signal

actions =
  Signal.mailbox Nop

inputs : Signal Model.Action
inputs = 
  let
    pressesWhenNotEditing =
      let codeToAction code =
        case code of
          0 -> Nothing
          8 -> Just Clear
          otherwise -> code |> Char.fromCode >> Just >> Edit >> Just
      in
        Keyboard.presses
        |> keepWhen (modelIsEditing.signal ~> not) 0
        |> Signal.map codeToAction
        |> Signal.Extra.filter Nop
    movement =
      let action dir altKey =
        case (dir.x, dir.y, altKey) of
          (0, 0, _) -> Nop
          (_, _, True) -> Insert dir
          (_, _, False) -> Move dir
      in
        Signal.Extra.passiveMap2 action Keyboard.arrows (Keyboard.isDown 18)
  in
    Signal.mergeMany (actions.signal :: [movement, pressesWhenNotEditing])

model : Signal Model.Model
model = 
  Signal.foldp Model.update Model.empty inputs

main : Signal Html.Html
main = 
  Signal.map (View.view actions.address) model
