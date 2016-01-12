module App where

import Keyboard
import Signal exposing (Signal, Address)
import Signal.Extra exposing ((~>))
import Model exposing (Action(..))
import View
import Html
import Html.Lazy
import Task
import StartApp
import Effects
import Solver

inputs : List (Signal Model.Action)
inputs = 
  [ Signal.Extra.passiveMap2
      (\{x,y} alt -> InputArrows {x = x, y = y, alt = alt})
      Keyboard.arrows
      (Keyboard.isDown 18)
  , Keyboard.presses ~> InputKeypress
  ]

port z3: String -- path to z3-emscripten

app : StartApp.App Model.Model
app =
  StartApp.start
    { init = 
        ( Model.empty
        , Solver.load z3 -- Start loading solver at start
          |> Task.toResult
          |> Task.map LoadSolver
          |> Effects.task
        )
    , view = Html.Lazy.lazy2 View.view
    , update = Model.update
    , inputs = inputs
    }

main : Signal Html.Html
main =
  app.html

port focus : Signal Bool
port focus =
  app.model ~> Model.isEditing |> Signal.dropRepeats

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
