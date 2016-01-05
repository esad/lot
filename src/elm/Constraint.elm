module Constraint (Constraint, parse, toSmtLibAssert) where

import Result
import Combine exposing (..)
import Combine.Infix exposing ((<*), (<?>))
import Combine.Num
import String

type Rel = Eq | NotEq | Lt | LtEq | Gt | GtEq 

type Op = Add | Sub | Mul | Div

type Expr = Const Int | Var String | Calc Expr Op Expr 
  
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
        Var v ->
          v
        Calc e1 op e2 ->
          sexp [opToString op, exprSexp e1, exprSexp e2]
  in
    sexp
      [ "assert"
      , sexp
        [ relToString rel
        , identifier
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

var : Parser Expr
var = 
  Var `map` regex "[a-zA-Z][a-zA-Z0-9]*" <?> "cell identifier or label"

expr : Parser Expr
expr = rec <| \() -> (Calc `map` mulExpr `andMap` addOp `andMap` expr) `or` mulExpr

mulExpr : Parser Expr
mulExpr = rec <| \() -> (Calc `map` factor `andMap` mulOp `andMap` mulExpr) `or` factor

factor : Parser Expr
factor = rec <| \() -> parens expr `or` constExpr `or` var |> tokenize

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