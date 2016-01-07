module Model (Model, Action, Action(..), empty, isEditing, update) where

import Sheet
import Cell exposing (Cell(..))
import Addr exposing (Addr, Direction(..))
import Char
import String
import Maybe exposing (andThen)
import Task
import Effects
import Solver
import Constraint

type alias Model = 
  { sheet : Sheet.Sheet
  , selection : Addr
  -- If Nothing, no cell is being edited. If Just String, then the string holds initial value
  -- of the edit box (which can be empty string). Only currently selected cell can be edited.
  , editing : Maybe String
  , solver: Maybe Solver.Solver -- available when z3 solver loads successfully (see LoadSolver action)
  }

empty : Model
empty =
  { sheet = Sheet.initialize 5 5
  , selection = Addr.fromColRow 0 0
  , editing = Nothing
  , solver = Nothing
  }

isEditing : Model -> Bool
isEditing model =
  model.editing /= Nothing

type Action
  = InputArrows { x: Int, y: Int, alt: Bool }
  | InputKeypress Char.KeyCode
  ---
  | LoadSolver (Maybe Solver.Solver)
  | Solve
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
      case action of 
        LoadSolver _ -> action -- solver object can't be shown w/o blowing the stack
        _ -> Debug.log "update" action 
    noFx model =
      (model, Effects.none)
    nop =
      noFx model
    anotherActionFx action model = 
      (model, Task.succeed action |> Effects.task)
  in
  case action of
    InputArrows a ->
      case (Addr.xy2dir a, isEditing model) of
        (Just Left, True) -> 
          nop
        (Just Right, True) ->
          nop
        (Just dir, _) ->
          anotherActionFx (if a.alt then Insert dir else Move dir) model
        (Nothing, _) ->
          nop
    InputKeypress key ->
      case (isEditing model, key) of
        (_, 0) ->
          nop
        (True, _) ->
          nop
        (False, 8) ->
          anotherActionFx Clear model
        (False, _) ->
          anotherActionFx (Edit (Just <| Char.fromCode key)) model
    ---
    LoadSolver solver ->
      noFx
        { model |
          solver = solver
        }
    Solve -> 
      case model.solver of
        Just solver ->  
          case Solver.solve model.sheet solver of
            Ok solution ->
              noFx
                { model |
                  sheet = solution
                }
            Err error ->
              nop
        Nothing ->
          let _ = Debug.log "No solver yet" in nop
    ---
    Clear ->
      let effect = case Sheet.get model.selection model.sheet of
        -- If we clear a constrained cell, we need to reevaluate
        Just (ConstrainedCell _) -> anotherActionFx Solve
        -- otherwise not
        _ -> noFx
      in
      effect
        { model |
          sheet = Sheet.update model.selection (always EmptyCell) model.sheet
        }
    Select addr ->
      noFx 
        { model | 
          selection = addr
        } 
    Commit addr str ->
      -- Try parsing constraints
      let (cell, effect) =
        case Constraint.parse str of
          Ok constraints ->
            (ConstrainedCell { constraints = constraints, solution = Nothing, source = str }, anotherActionFx Solve)
          Err _ ->
            (TextCell str, noFx)
      in
      effect
        { model |
          editing = Nothing,
          sheet = Sheet.update addr (always cell) model.sheet,
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
          editing = Nothing
        }
    Edit char ->
      noFx
        { model | 
          editing = 
            Maybe.oneOf 
              [ char `andThen` (String.fromChar >> Just)
              , (Sheet.get model.selection model.sheet) `andThen` Cell.editString
              , Just ""
              ]
        }
    Move direction ->
      noFx
        { model |
          selection = Sheet.move model.selection direction model.sheet,
          editing = Nothing
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
