module Model (Model, Action, Action(..), Mode(..), empty, update) where

import Sheet exposing (Cell(..))
import Addr exposing (Addr, Direction(..))
import Char
import String
import Maybe exposing (andThen)
import Task
import Effects
import Solver

type Mode
  --            Nothing: spreadsheet in navigation mode
  --            Just String: spreadsheet in edit mode with the current edit value
  = Spreadsheet (Maybe String)
  | Code 

type alias Model = 
  { sheet : Sheet.Sheet
  , selection : Addr
  , mode : Mode
  , code : String
  }

empty : Model
empty =
  { sheet = Sheet.initialize 5 5
  , selection = Addr.fromColRow 0 0
  , mode = Spreadsheet Nothing
  , code = "-- Welcome to Lot"
  }

type Action
  = InputArrows { x: Int, y: Int, alt: Bool }
  | InputKeypress Char.KeyCode
  | InputCode (Maybe String)
  | InputCodeFocused Bool
  ---
  | LoadSolver (Maybe Solver.Solver)
  ---
  | Select Addr -- Direct selection of the cell at given address
  | Move Addr.Direction -- Keyboard movement in this relative direction
  | Insert Addr.Direction -- Insert row/col in this direction
  | Edit (Maybe Char) -- Start editing. Can be triggerered by a character pressed by user (Just Char) or by doubleclick (Nothing)
  | Commit Addr String
  | Cancel
  | Clear -- updates selected cell to an empty cell

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  let
    action =
      Debug.log "update" action
    noFx model =
      (model, Effects.none)
    nop =
      noFx model
    anotherActionFx action model = 
      (model, Task.succeed action |> Effects.task)
  in
  case action of
    InputArrows a ->
      case (model.mode, Addr.xy2dir a) of
        (_, Nothing) ->
          nop
        (Code, _) ->
          nop
        (Spreadsheet _, Just dir) ->
          anotherActionFx (if a.alt then Insert dir else Move dir) model
    InputKeypress key ->
      case (model.mode, key) of
        (_, 0) ->
          nop
        (Code, 27) ->
          -- Switch focus back to spreadsheet
          noFx 
            { model |
              mode = Spreadsheet Nothing
            }
        (Code, _) ->
          nop
        (Spreadsheet (Just _), _) ->
          nop
        (Spreadsheet Nothing, 8) ->
          anotherActionFx Clear model
        (Spreadsheet Nothing, 27) ->
          -- Switch focus to code
          noFx
            { model |
              mode = Code
            }
        (Spreadsheet Nothing, _) ->
          anotherActionFx (Edit (Just <| Char.fromCode key)) model
    InputCodeFocused focus ->
      noFx
        { model |
          mode = if focus then Code else Spreadsheet Nothing
        }
    InputCode Nothing ->
      nop
    InputCode (Just code) ->
      noFx 
        { model | 
          code = code 
        }
    ---
    LoadSolver solver ->
      nop
    ---
    Clear ->
      noFx
        { model |
          sheet = Sheet.update model.selection (always (TextCell "")) model.sheet
        }
    Select addr ->
      noFx 
        { model | 
          selection = addr
        } 
    Commit addr str ->
      noFx
        { model |
          mode = Spreadsheet Nothing,
          sheet = Sheet.update addr (always (TextCell str)) model.sheet,
          selection =
            if addr == model.selection then
              -- Enter key was pressed, advance selection by one
              Sheet.move model.selection Down model.sheet
            else
              model.selection
        }
    Cancel ->
      noFx 
        { model | 
          mode = Spreadsheet Nothing
        }
    Edit char ->
      noFx
        { model | 
          mode = 
            Maybe.oneOf 
              [ char `andThen` (String.fromChar >> Just)
              , (Sheet.get model.selection model.sheet) `andThen` Sheet.cell2str
              , Just ""
              ]
            |> Spreadsheet
      }
    Move direction ->
      noFx
        { model |
          selection = Sheet.move model.selection direction model.sheet,
          mode = Spreadsheet Nothing
        }
    Insert direction ->
      let
        sheet = 
          if direction == Left || direction == Right  then
            Sheet.insertCol (Addr.col model.selection) model.sheet
          else
            Sheet.insertRow (Addr.row model.selection) model.sheet
        selection =
          if direction == Right || direction == Down then
            Sheet.move model.selection direction sheet
          else
            model.selection
      in
      noFx
        { model |
          selection = selection,
          sheet = sheet 
        }
