module Solver where

import Native.Solver
import Task

type Solver = Solver

load : String -> Task.Task x Solver
load z3_url =
  Native.Solver.load z3_url