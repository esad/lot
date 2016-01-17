module Cell (Cell, Cell(..), fromString, toString, editString) where

import String

import Constraint

type Cell
  = EmptyCell
  | TextCell String
  | DerivedCell Int -- a cell that was constrained to this value by another cell (or a global constraint)
  | ConstrainedCell
    { solution : Maybe Int
    , constraints : List Constraint.Constraint
    , source : String
    --, dependencies : List String -- list of cell identifiers this cell depends on
    }

fromString : String -> Cell
fromString str =
  case String.trim str |> String.isEmpty of
    True ->
      EmptyCell
    False -> 
      case Constraint.parse str of
        Ok (x::xs as constraints) ->
          ConstrainedCell { constraints = constraints, solution = Nothing, source = str }
        Ok [] ->
          TextCell str
        Err _ ->
          TextCell str

toString : Cell -> Maybe String
toString cell =
  case cell of
    EmptyCell ->
      Nothing
    TextCell text ->
      Just text
    DerivedCell i ->
      Just <| Basics.toString i
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

