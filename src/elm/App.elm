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

-- Other inbound ports

port codeFocused : Signal Bool
port code: Signal (Maybe String)

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
    { init = (Model.empty, Effects.none)
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
