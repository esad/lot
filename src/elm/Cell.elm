module Cell (Cell, Cell(..), fromString, toString, editString) where

import String
import Set
import Constraint

type Cell
  = EmptyCell
  | TextCell String
  | DerivedCell Float -- a cell that was constrained to this value by another cell (or a global constraint)
  | ConstrainedCell
    { solution : Maybe Float
    , constraints : List Constraint.Constraint
     -- precalculated set of cell identifiers this cell's constraints are referring to, possibly empty
    , dependencies : Set.Set String
    , hasConst: Bool
    , source : String
    }

fromString : String -> Cell
fromString str =
  case String.trim str |> String.isEmpty of
    True ->
      EmptyCell
    False -> 
      case Constraint.parse str of
        Ok (x::xs as constraints) ->
          ConstrainedCell 
            { solution = Nothing
            , constraints = constraints
            , dependencies = List.foldl (\c ds -> Constraint.dependencies c `Set.union` ds) Set.empty constraints
            , hasConst = List.any Constraint.isConst constraints
            , source = str
            }
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

