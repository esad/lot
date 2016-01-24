module Model (Model, Action, Action(..), Focus(..), empty, isEditing, update) where

import Sheet
import Cell exposing (Cell(..))
import Addr exposing (Addr, Direction(..))
import Char
import String
import Maybe exposing (andThen)
import Task
import Effects
import Solver
import Constraint exposing (Context(..))
import Tableau

type Focus = Spreadsheet | Globals

type alias Model = 
  { focus : Focus
  , sheet : Sheet.Sheet
  , selection : Addr
  -- If Nothing, no cell is being edited. If Just String, then the string holds initial value
  -- of the edit box (which can be empty string). Only currently selected cell can be edited.
  , editing : Maybe String
  , tableau: Tableau.Tableau
  , solver: Maybe (Result String Solver.Solver) -- available when z3 solver loads successfully (see LoadSolver action)
  }

empty : Model
empty =
  { focus = Spreadsheet
  , sheet = Sheet.initialize 5 5
  , selection = Addr.fromColRow 0 0
  , editing = Nothing
  , tableau = Tableau.empty
  , solver = Nothing
  }

isEditing : Model -> Bool
isEditing model =
  model.editing /= Nothing

type Action
  = InputArrows { x: Int, y: Int, alt: Bool }
  | InputKeypress Char.KeyCode
  | SwitchFocus Focus
  ---
  | LoadSolver (Result String Solver.Solver)
  | Solve
  | AddConstraint String
  | DropConstraint Int
  ---
  | Select Addr -- Direct selection of the cell at given address
  | Move Addr.Direction -- Keyboard movement in this relative direction
  | Insert Addr.Direction -- Insert row/col in this direction
  | Edit (Maybe Char) -- Start editing. Can be triggerered by a character pressed by user (Just Char) or by doubleclick (Nothing)
  | Commit Addr String
  | Cancel
  | Clear -- updates selected cell to an empty cell
  ---
  | Nop

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
    Nop ->
      nop
    SwitchFocus f ->
      noFx
        { model | 
          focus = f
        }
    InputArrows a ->
      case model.focus of
        Spreadsheet ->
          case (Addr.xy2dir a, isEditing model) of
            (Just Left, True) -> 
              nop
            (Just Right, True) ->
              nop
            (Just dir, _) ->
              anotherActionFx (if a.alt then Insert dir else Move dir) model
            (Nothing, _) ->
              nop
        Globals ->
          nop
    InputKeypress key ->
      case model.focus of
        Spreadsheet ->
          case (isEditing model, key) of
            (_, 0) ->
              nop
            (True, _) ->
              nop
            (False, 8) ->
              anotherActionFx Clear model
            (False, _) ->
              -- Enter should be like double-click
              anotherActionFx (Edit <| if key == 13 then Nothing else Just <| Char.fromCode key) model
        Globals ->
          nop
    ---
    LoadSolver result ->
      noFx
        { model |
          solver = Just result
        }
    Solve -> 
      case model.solver of
        Just (Ok solver) ->  
          case Solver.solve model.sheet model.tableau solver of
            Ok solution ->
              noFx
                { model |
                  sheet = solution
                }
            Err error ->
              nop
        _ ->
          let _ = Debug.log "No solver available" in nop
    AddConstraint str ->
      case Tableau.parse GlobalContext str of
        Ok t ->
          anotherActionFx Solve
            { model | 
              tableau = Tableau.append model.tableau t
            }
        Err error ->
          nop
    DropConstraint i ->
      anotherActionFx Solve 
        { model |
          tableau = Tableau.dropAt i model.tableau
        }
    ---
    Clear ->
      let
        clearSheet = Sheet.update model.selection (always EmptyCell) model.sheet
      in
      case Sheet.get model.selection model.sheet of
        Just (ResultCell _) ->
          -- Do not allow clearing result cells
          nop
        _ ->
          -- otherwise, clear the cell but don't reevaluate
          noFx { model | sheet = clearSheet }
    Select addr ->
      noFx 
        { model
        | focus = Spreadsheet
        , selection = addr
        }
    Commit addr str ->
      let
        cell = 
          Sheet.get addr model.sheet
        id = 
          Addr.toIdentifier addr
        (newCell, newTableau) =
          case String.trim str |> String.isEmpty of
            True ->
              (EmptyCell, [])
            False ->
              case Tableau.parse (CellContext id) str of
                Err _ ->
                  (TextCell str, [])
                Ok t ->
                  (ResultCell Nothing, t)
        reevaluate = 
          case (cell, newCell) of
            (Just (ResultCell _), _) -> True
            (_ , ResultCell _) -> True
            _ -> False
      in
        (if reevaluate then anotherActionFx Solve else noFx)
        { model
          | editing = Nothing
          , sheet = Sheet.update addr (always newCell) model.sheet
          , selection =
              if addr == model.selection then
                -- Enter key was pressed, advance selection by one
                Sheet.move model.selection Down model.sheet
              else
                model.selection
          , tableau =
              if reevaluate then
                model.tableau 
                |> Tableau.dropCell id
                |> flip Tableau.append newTableau
              else
                model.tableau
        }
    Cancel ->
      noFx 
        { model | 
          editing = Nothing
        }
    Edit char ->
      let
        id = Addr.toIdentifier model.selection
      in
      noFx
        { model | 
          editing = 
            Maybe.oneOf 
              [ char `andThen` (String.fromChar >> Just)
              , (Sheet.get model.selection model.sheet) `andThen` (\cell ->
                case cell of 
                  TextCell t -> Just t
                  ResultCell _ -> Just <| Tableau.source id model.tableau
                  _ -> Just ""
              )
              , Just "?"
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
