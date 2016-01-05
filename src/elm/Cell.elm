module Cell (Cell, Cell(..), toString) where

import Constraint

type Cell
  = EmptyCell
  | TextCell String
  | ConstrainedCell
    { solution : Maybe Int
    , constraints : List Constraint.Constraint
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
