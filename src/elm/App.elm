module App where

import Keyboard
import Signal exposing (Signal, Address)
import Signal.Extra exposing ((~>))
import Model exposing (Action(..), Mode(..))
import View
import Html
import Html.Lazy
import Task
import StartApp
import Effects
import Solver

-- Other inbound ports

port codeFocused : Signal Bool
port code: Signal (Maybe String)
port z3: String -- path to z3-emscripten

inputs : List (Signal Model.Action)
inputs = 
  [ Signal.Extra.passiveMap2
      (\{x,y} alt -> InputArrows {x = x, y = y, alt = alt})
      Keyboard.arrows
      (Keyboard.isDown 18)
  , Keyboard.presses ~> InputKeypress
  , codeFocused ~> InputCodeFocused
  , code ~> InputCode
  ]

app : StartApp.App Model.Model
app =
  StartApp.start
    { init = 
        ( Model.empty
        -- Start loading solver at start
        , Solver.load z3 
          |> Task.toMaybe
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


port focus : Signal String
port focus =
  let focusString model =
    case model.mode of
      Code -> "code"
      Spreadsheet Nothing -> "sheet"
      Spreadsheet (Just _) -> "cellInput"
  in
  app.model ~> focusString

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
