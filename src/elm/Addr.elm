module Addr (Addr, row, col, fromColRow, add, toIdentifier, fromIdentifier, rowIdentifier, colIdentifier) where

import Array
import Char
import String
import Regex
import Maybe exposing (andThen)

type Addr = Addr Int Int

row : Addr -> Int
row (Addr _ x) =
  x

col : Addr -> Int
col (Addr x _) = 
  x

fromColRow : Int -> Int -> Addr
fromColRow col row =
  Addr col row

add : Addr -> Addr -> Addr
add a b =
  fromColRow (col a + col b) (row a + row b)

alphabet : Array.Array Char
alphabet = 
  Array.initialize 26 (((+) 65) >> Char.fromCode)

toBase : Int -> Int -> List Int
toBase b v = 
  let toBase' list v =
    if v == 0 then
      list
    else
      toBase' (v `rem` b :: list) (v // b)
  in
    if v == 0 then
      [0]
    else
      toBase' [] v

fromBase : Int -> List Int -> Int
fromBase b digits =
  List.reverse digits
  |> List.indexedMap (\i n -> b ^ i * n)
  |> List.sum

colIdentifier : Int -> String
colIdentifier col =
  col
  |> toBase 26
  |> List.filterMap (\n -> Array.get n alphabet)
  |> String.fromList

rowIdentifier : Int -> String
rowIdentifier row =
  row + 1 |> toString 

toIdentifier : Addr -> String
toIdentifier addr =
  (col addr |> colIdentifier)
  ++
  (row addr |> rowIdentifier)

fromIdentifier : String -> Maybe Addr
fromIdentifier id =
  let
    parseCol id =
      id 
      |> String.toList
      |> List.map (Char.toCode >> (flip (-) 65))
      |> fromBase 26
    parseRow id =
      case String.toInt id of
        Ok r -> Just (r - 1)
        _ -> Nothing
    re =
      Regex.regex "^([A-Z]+)([1-9][0-9]*)"
    submatches =
      id 
      |> String.toUpper
      |> Regex.find Regex.All re
      |> List.concatMap .submatches
      |> List.filterMap identity
  in
    case submatches of
      (colId :: rowId :: _) ->
        case (parseCol colId, parseRow rowId) of 
          (col, Just row) -> fromColRow col row |> Just
          _ -> Nothing
      _ -> Nothing
