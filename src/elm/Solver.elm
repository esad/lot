module Solver where

import Native.Solver
import Task
import Sheet
import Addr
import Cell exposing (Cell(..))
import Constraint
import String

type Solver = Solver

load : String -> Task.Task x Solver
load z3_url =
  Native.Solver.load z3_url

solve : Sheet.Sheet -> Solver -> Result String (Sheet.Sheet)
solve sheet solver =
  let
    (ids, asserts) =
      sheet
      |> Sheet.toList
      |> List.indexedMap (\row cells -> List.indexedMap (\col cell -> 
        case cell of
          Cell.ConstrainedCell {constraints} ->
            let
              id = Addr.fromColRow col row |> Addr.toIdentifier
              asserts = List.map (Constraint.toSmtLibAssert id) constraints
            in
              Just (id, asserts)
          _ ->
            Nothing
      ) cells)
      |> List.concat
      |> List.filterMap identity
      |> List.unzip
    smt2 =
      asserts
      |> List.concat
      |> String.join "\n"
  in
    case Native.Solver.solve solver smt2 ids of
      Ok solutions ->
        List.foldl (\(id,value) s -> 
          let _ = Debug.log ("id" ++ id ++ " for ") value  in
          case Addr.fromIdentifier id of
            Just a -> 
              Sheet.update a (\cell ->
                case cell of 
                  ConstrainedCell c ->
                    ConstrainedCell { c | solution = Just value }
                  _ ->
                    Debug.log "Received value for non-constrained cell?" cell
              ) s
            Nothing ->
              Debug.log ("Unknown identifier " ++ id ++ " for ") s
        ) sheet solutions
        |> Debug.log "Solved to:"
        |> Ok
      Err err as error ->
        let _ = Debug.log ("Error solving "++err) sheet
        in Err err


    