module Solver where

import Native.Solver
import Task
import Sheet
import Addr
import Cell exposing (Cell(..))
import Constraint
import Identifier
import String
import Set
import Maybe exposing (andThen)
import Dict

type Solver = Solver

load : String -> Task.Task x Solver
load z3_url =
  Native.Solver.load z3_url

solve : Sheet.Sheet -> Solver -> Result String (Sheet.Sheet)
solve sheet solver =
  let
    (ids, asserts) =
      Sheet.fold 
        (\(addr, cell) (ids, asserts) ->
          case cell of
            Cell.ConstrainedCell {constraints} ->
              let
                cellId = Addr.toIdentifier addr
                cellAsserts = List.map (Constraint.toSmtLibAssert cellId) constraints
                cellDeps = List.concatMap (Constraint.dependencies) constraints
              in
                (cellId :: cellDeps ++ ids, cellAsserts ++ asserts)
            _ ->
              (ids, asserts)        
        )
        ([],[])
        sheet
    uniqueIds = 
      ids 
      |> List.map Identifier.toString
      |> Set.fromList 
      |> Set.toList
    smt2 =
      String.join "\n" asserts
    result =
      case asserts of
        [] -> -- When there are no assertions, do not call the solver
          Ok []
        _ ->
          Native.Solver.solve solver smt2 uniqueIds
  in
    case result of
      -- Ok Dict String Int
      Ok solutions ->
        let
          solutionsDict = Dict.fromList solutions
        in
        sheet
        |> Sheet.map (\addr cell ->
          let 
            key = addr |> Addr.toIdentifier |> Identifier.toString
          in
          case (cell, Dict.get key solutionsDict) of
            (ConstrainedCell c, value) ->
              ConstrainedCell { c | solution = value } -- TODO: catch nothing as "no solution"
            (DerivedCell _, Nothing) ->
              EmptyCell
            (_, Just i) ->
              DerivedCell i
            (_, Nothing) ->
              cell
        )
        |> Debug.log "Solved to:"
        |> Ok
      Err err as error ->
        let _ = Debug.log ("Error solving "++err) sheet
        in Err err
    