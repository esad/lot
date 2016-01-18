module Constraint (Constraint, parse, dependencies, toSmtLibAssert, isConst) where

import Result
import Combine exposing (..)
import Combine.Infix exposing ((<*), (<?>))
import Combine.Num
import String
import Set exposing (union)

type Rel = Eq | NotEq | Lt | LtEq | Gt | GtEq 

type Op = Add | Sub | Mul | Div

type Expr = Const Float | Id String | Calc Op Expr Expr 
  
type Constraint = Constraint Rel Expr

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

-- Returns true if constraint relational operator is "=" and it has no deps
isConst : Constraint -> Bool
isConst c =
  case c of
    Constraint Eq _ ->
      c |> dependencies |> Set.isEmpty 
    _ ->
      False

-- Returns a set of identifiers this constraint depends on
dependencies : Constraint -> Set.Set String
dependencies (Constraint rel expr) =
  let depExpr e =
    case e of 
      Const _ -> Set.empty
      Id id -> Set.singleton id
      Calc _ e1 e2 -> depExpr e1 `union` depExpr e2
  in
    depExpr expr

--- Output

toSmtLibAssert : String -> Constraint -> String
toSmtLibAssert identifier (Constraint rel exp) =
  let
    sexp xs =
      "(" ++ (String.join " " xs) ++ ")"
    exprSexp e =
      case e of 
        Const c ->
          toString c
        Id i ->
          i
        Calc op e1 e2 ->
          sexp [opToString op, exprSexp e1, exprSexp e2]
  in
    sexp
      [ "assert"
      , case rel of 
          NotEq ->
            -- != must be transformed into (not (= ...))
            sexp ["not", sexp [relToString Eq, identifier, exprSexp exp]]
          _ ->
            sexp [relToString rel, identifier, exprSexp exp]
      ]

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
  Id `map` regex "[a-zA-Z][a-zA-Z0-9]*" <?> "cell identifier"

expr : Parser Expr
expr = rec <| \() -> term `chainl` (Calc `map` addOp)

term : Parser Expr
term = rec <| \() -> factor `chainl` (Calc `map` mulOp)

factor : Parser Expr
factor = rec <| \() -> parens expr `or` constExpr `or` identifier |> tokenize

constraint : Parser Constraint
constraint =
  Constraint Eq `map` constExpr `or` -- "300" -> "= 300"
  (Constraint `map` (tokenize rel) `andMap` expr) -- regular constraint

constraints : Parser (List Constraint)
constraints =
  sepBy (string ";") constraint 

parse : String -> Result (List String) (List Constraint)
parse str =
  Combine.parse (constraints <* end) str
  |> fst