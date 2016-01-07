module Cell (Cell, Cell(..), toString, editString) where

import Constraint

type Cell
  = EmptyCell
  | TextCell String
  | ConstrainedCell
    { solution : Maybe Int
    , constraints : List Constraint.Constraint
    , source : String
    --, dependencies : List String -- list of cell identifiers this cell depends on
    }

toString : Cell -> Maybe String
toString cell =
  case cell of
    EmptyCell ->
      Nothing
    TextCell text ->
      Just text
    ConstrainedCell {solution} ->
      case solution of
        Nothing -> Just "..."
        Just i -> Just <| Basics.toString i

editString : Cell -> Maybe String
editString cell =
  case cell of
    ConstrainedCell {source} ->
      Just source
    _ ->
      toString cell

