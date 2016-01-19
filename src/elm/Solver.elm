module Solver where

import Native.Solver
import Task
import Sheet
import Addr
import Cell exposing (Cell(..))
import Tableau
import String
import Dict

type Solver = Solver

load : String -> Task.Task x Solver
load z3_url =
  Native.Solver.load z3_url

solve : Sheet.Sheet -> Tableau.Tableau -> Solver -> Result String (Sheet.Sheet)
solve sheet tableau solver =
  let
    result =
      case Tableau.toSmt tableau of
        Nothing -> -- When there are no  no assertions, do not call the solver
          Ok []
        Just (program, ids) ->
          Native.Solver.solve solver program ids
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
            (ResultCell _, Nothing) ->
              EmptyCell
            (_, Just i) ->
              ResultCell (Just i)
            (_, Nothing) ->
              cell
        )
        |> Debug.log "Solved to:"
        |> Ok
      Err err as error ->
        let _ = Debug.log ("Error solving "++err) sheet
        in Err err
    