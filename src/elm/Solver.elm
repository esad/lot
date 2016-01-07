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
  in
    case Native.Solver.solve solver smt2 uniqueIds of
      Ok solutions ->
        List.foldl (\(id,value) s -> 
          let _ = Debug.log ("id" ++ id ++ " for ") value  in
          case Addr.fromIdentifier <| Identifier.fromString id of
            Just a -> 
              Sheet.update a (\cell ->
                case cell of 
                  ConstrainedCell c ->
                    ConstrainedCell { c | solution = Just value }
                  _ ->
                    TextCell (toString value)
              ) s
            Nothing ->
              Debug.log ("Unknown identifier " ++ id ++ " for ") s
        ) sheet solutions
        |> Debug.log "Solved to:"
        |> Ok
      Err err as error ->
        let _ = Debug.log ("Error solving "++err) sheet
        in Err err


    