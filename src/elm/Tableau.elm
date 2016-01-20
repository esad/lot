module Tableau (Tableau, empty, parse, append, dropCell, toSmt, source) where

import String
import Result
import Set

import Constraint exposing (Constraint, Context(..))

type alias Tableau = 
  List (String {- source text -}, Constraint)
  

empty : Tableau
empty =
  []


sep : String
sep = ";"

-- Returns smt program + list of all identifiers occuring in the program
-- or Nothing if there are no constraints in the tableau
toSmt : Tableau -> Maybe (String, List String)
toSmt t =
  let
    (asserts, ids) =
      List.foldl (\(_, constraint) (asserts, identifiers) -> 
        ( Constraint.toSmtAssert constraint :: asserts
        , Set.union (Constraint.identifiers constraint) identifiers
        )
      ) ([],Set.empty) t
  in
    case t of
      [] -> Nothing
      _ -> Just (String.join "\n" asserts, Set.toList ids)


-- Parses a string containing one or many constraints separated by ";" within a context
-- (Nothing if it's a global constraint, Just String if contraints belong to cell with the given identifier)
-- If all of the constraints are parsed successfully, an Ok Tableau is returned, Err with list of
-- parsing errors otherwise.
parse : Constraint.Context -> String -> Result (List String) Tableau
parse context source =
  let
    parse str =
      Result.map (\c -> (str,c)) (Constraint.parse context str)
  in
  source
  |> String.split sep
  |> List.map String.trim
  |> List.filter (String.isEmpty >> not)
  |> List.foldr (parse >> Result.map2 (::)) (Ok [])


append : Tableau -> Tableau -> Tableau
append = (++)

-- Returns a new tableau without all constraints belonging to given cell identifier
dropCell : String -> Tableau -> Tableau
dropCell id t =
  List.filter (snd >> (Constraint.hasContext (CellContext id)) >> not) t

-- Returns concatenated sources of all constraints related to this cell
source : String -> Tableau -> String
source id t =
  t
  |> List.filterMap (\(source, c) -> if Constraint.hasContext (CellContext id) c then Just source else Nothing)
  |> String.join (sep ++ " ")
