module Constraint (Constraint, parse, dependencies, toSmtLibAssert) where

import Identifier exposing (Identifier)

import Result
import Combine exposing (..)
import Combine.Infix exposing ((<*), (<?>))
import Combine.Num
import String

type Rel = Eq | NotEq | Lt | LtEq | Gt | GtEq 

type Op = Add | Sub | Mul | Div

type Expr = Const Int | Id Identifier | Calc Expr Op Expr 
  
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

-- Returns a list of identifiers this constraint depends on

dependencies : Constraint -> List Identifier
dependencies (Constraint rel expr) =
  let depExpr e =
    case e of 
      Const _ -> []
      Id id -> [id]
      Calc e1 _ e2 -> depExpr e1 ++ depExpr e2
  in
    depExpr expr

--- Output

toSmtLibAssert : Identifier -> Constraint -> String
toSmtLibAssert identifier (Constraint rel exp) =
  let
    sexp xs =
      "(" ++ (String.join " " xs) ++ ")"
    exprSexp e =
      case e of 
        Const c ->
          toString c
        Id i ->
          Identifier.toString i
        Calc e1 op e2 ->
          sexp [opToString op, exprSexp e1, exprSexp e2]
  in
    sexp
      [ "assert"
      , sexp
        [ relToString rel
        , Identifier.toString identifier
        , exprSexp exp
        ]
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
  map Const Combine.Num.int
  |> tokenize

identifier : Parser Expr
identifier = 
  (Id << Identifier.fromString) `map` regex "[a-zA-Z][a-zA-Z0-9]*" <?> "cell identifier"

expr : Parser Expr
expr = rec <| \() -> (Calc `map` mulExpr `andMap` addOp `andMap` expr) `or` mulExpr

mulExpr : Parser Expr
mulExpr = rec <| \() -> (Calc `map` factor `andMap` mulOp `andMap` mulExpr) `or` factor

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