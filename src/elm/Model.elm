module Model (Model, Action, Action(..), empty, update, isEditing) where

import Sheet exposing (Cell(..))
import Addr exposing (Addr, Direction(..))
import Char
import String
import Maybe exposing (andThen)

type alias Model = 
  { sheet : Sheet.Sheet
  , selection : Addr
  -- If Nothing, no cell is being edited. If Just String, then the string holds initial value
  -- of the edit box (which can be empty string). Only currently selected cell can be edited.
  , editing : Maybe String 
  }

empty : Model
empty =
  { sheet = Sheet.initialize 5 5
  , selection = Addr.fromColRow 0 0
  , editing = Nothing
  }

isEditing : Model -> Bool
isEditing model =
  model.editing /= Nothing

type Action
  = Nop -- do nothing
  | Select Addr -- Direct selection of the cell at given address
  | Move Addr.Direction -- Keyboard movement in this relative direction
  | Insert Addr.Direction -- Insert row/col in this direction
  | Edit (Maybe Char) -- Start editing. Can be triggerered by a character pressed by user (Just Char) or by doubleclick (Nothing)
  | Commit Addr String
  | Cancel
  | Clear -- updates selection with nil value

update : Action -> Model -> Model
update action model =
  let action =
    Debug.log "update" action
  in
  case action of
    Nop ->
      model
    Clear ->
      { model |
          sheet = Sheet.update model.selection (always (TextCell "")) model.sheet
      }
    Select addr ->
      { model | 
          selection = addr
      } 
    Commit addr str ->
      { model |
          editing = Nothing,
          sheet = Sheet.update addr (always (TextCell str)) model.sheet,
          selection =
            if addr == model.selection then
              -- Enter key was pressed, advance selection by one
              Sheet.move model.selection Down model.sheet
            else
              model.selection
      }
    Cancel ->
      { model |
          editing = Nothing 
      }
    Edit char ->
      { model | 
          editing = 
            Maybe.oneOf 
              [ char `andThen` (String.fromChar >> Just)
              , (Sheet.get model.selection model.sheet) `andThen` Sheet.cell2str
              , Just ""
              ]
      }
    Move direction ->
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
        { model |
            selection = selection,
            sheet = sheet 
        }
