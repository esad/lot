module Constraint (Constraint, Context(..), parse, toSmtAssert, identifiers, hasContext, toString) where

import Result
import Combine exposing (..)
import Combine.Infix exposing ((<*), (<?>))
import Combine.Num
import String
import Set exposing (union)

type Rel = Eq | NotEq | Lt | LtEq | Gt | GtEq 

type Op = Add | Sub | Mul | Div

type Expr = Const Float | Id String | Calc Op Expr Expr 
  
type Constraint = Constraint Expr Rel Expr

type Context = GlobalContext | CellContext String

relToString : Rel -> String
relToString rel =
  case rel of
    Eq    -> "="
    NotEq -> "!="
    Lt    -> "<"
    LtEq  -> "<="
    Gt    -> ">"
    GtEq  -> ">="

opToString : Op -> String
opToString op =
  case op of
    Add   -> "+"
    Sub   -> "-"
    Mul   -> "*"
    Div   -> "/"

opPriority : Op -> Int
opPriority op =
  case op of
    Add -> 0
    Sub -> 0
    Mul -> 1
    Div -> 1

toSmtAssert : Constraint -> String
toSmtAssert (Constraint e1 rel e2) =
  let
    sexp xs =
      "(" ++ (String.join " " xs) ++ ")"
    exprSexp e =
      case e of 
        Const c -> Basics.toString c
        Id i -> i
        Calc op e1 e2 -> sexp [opToString op, exprSexp e1, exprSexp e2]
  in
    sexp
      [ "assert"
      , case rel of 
          NotEq ->
            -- != must be transformed into (not (= ...))
            sexp ["not", sexp [relToString Eq, exprSexp e1, exprSexp e2]]
          _ ->
            sexp [relToString rel, exprSexp e1, exprSexp e2]
      ]

identifiers : Constraint -> Set.Set String
identifiers (Constraint e1 rel e2) =
  let
    exprIdentifiers e =
      case e of 
        Const _ -> Set.empty
        Id i -> Set.singleton i
        Calc op e1 e2 -> exprIdentifiers e1 `union` exprIdentifiers e2
  in
    exprIdentifiers e1 `union` exprIdentifiers e2
        
-- Returns True if the constraint was defined for the given context
hasContext : Context -> Constraint -> Bool
hasContext context (Constraint e1 _ _) =
  case (context,e1) of
    (CellContext cellId, Id id) -> cellId == id
    (CellContext _, _) -> False
    (GlobalContext, _) -> True


-- Convert a constraint back to string, trying to be clever about parenthesis
-- If a supplied context matches the cell then the first part of expression before relation operator is left out
-- i.e. a1 = 500 -> "= 500"
toString : Context -> Constraint -> String
toString context (Constraint e1 rel e2 as constraint) =
  let
    parenthesize str = 
      "(" ++ str ++ ")"
    exprToString e ancestorOp =
      case e of
        Const c -> Basics.toString c
        Id i -> i
        Calc op e1 e2 ->
          let
            result = exprToString e1 (Just op) ++ opToString op ++ exprToString e2 (Just op)
          in
          if opPriority (Maybe.withDefault op ancestorOp) > opPriority op then 
            parenthesize result
          else
            result
  in
    (if context /= GlobalContext && hasContext context constraint then
      ""
    else 
      exprToString e1 Nothing ++ " "
    )
    ++
    relToString rel ++ " " ++ exprToString e2 Nothing
    
--- Parsing

tokenize : Parser a -> Parser a
tokenize p =
  let
    ws = regex "[ \t\r\n]*"
  in
    between ws ws p 

makeParser : List a -> (a -> String) -> Parser a
makeParser results transformer =
  List.map (\r -> (always r) `map` string (transformer r)) results
  |> choice
  
rel : Parser Rel
rel =
  makeParser [Eq, NotEq, LtEq, Lt, GtEq, Gt] relToString

mulOp : Parser Op
mulOp =
  makeParser [Mul, Div] opToString

addOp : Parser Op
addOp =
  makeParser [Add, Sub] opToString

constExpr : Parser Expr
constExpr = 
  let
    float = map Const Combine.Num.float
    int = map (Const << toFloat) Combine.Num.int
  in
  float `or` int
  |> tokenize

identifier : Parser Expr
identifier = 
  Id `map` regex "[a-zA-Z][a-zA-Z0-9]*" <?> "expected a cell identifier"

expr : Parser Expr
expr = rec <| \() -> term `chainl` (Calc `map` addOp)

term : Parser Expr
term = rec <| \() -> factor `chainl` (Calc `map` mulOp)

factor : Parser Expr
factor = rec <| \() -> parens expr `or` constExpr `or` identifier |> tokenize

-- We use this parser when parsing constraints type in the context of a cell.
-- The left part of the constraint is then set to (Id cellIdentifier).
cellConstraint : String -> Parser Constraint
cellConstraint id =
  Constraint (Id id) Eq `map` constExpr `or` -- "300" -> "= 300"
  ((Constraint (Id id)) `map` (tokenize rel) `andMap` expr) -- regular constraint

-- Otherwise, we use this global constraint parser where left side can be any expression:
constraint : Parser Constraint
constraint =
  Constraint `map` expr `andMap` (tokenize rel) `andMap` expr

-- Parses a constraint for cell (Just String) or global (Nothing)
parse : Context -> String -> Result (List String) Constraint
parse context str =
  let parser =
    case context of
      GlobalContext ->
        constraint
      CellContext id ->
        cellConstraint id
  in
  Combine.parse (parser <* end) str
  |> fst
