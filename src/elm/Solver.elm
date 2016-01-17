module Solver where

import Native.Solver
import Task
import Sheet
import Addr
import Cell exposing (Cell(..))
import Constraint
import String
import Set exposing (union)
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
            Cell.ConstrainedCell {constraints, dependencies} ->
              let
                cellId = Addr.toIdentifier addr
                cellAsserts = List.map (Constraint.toSmtLibAssert cellId) constraints
              in
                (Set.singleton cellId `union` dependencies `union` ids, cellAsserts ++ asserts)
            _ ->
              (ids, asserts)        
        )
        (Set.empty,[])
        sheet
    smt2 =
      String.join "\n" asserts
    result =
      case asserts of
        [] -> -- When there are no assertions, do not call the solver
          Ok []
        _ ->
          Native.Solver.solve solver smt2 (ids |> Set.toList)
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
            key = addr |> Addr.toIdentifier
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
    