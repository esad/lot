module Addr (Addr, row, col, fromColRow, add, identifier, rowIdentifier, colIdentifier) where

import Array
import Char
import String

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

colIdentifier : Int -> String
colIdentifier col =
  col
  |> toBase (Array.length alphabet)
  |> List.filterMap (\n -> Array.get n alphabet)
  |> String.fromList

rowIdentifier : Int -> String
rowIdentifier row =
  row + 1 |> toString 

identifier : Addr -> String
identifier addr =
  (col addr |> colIdentifier)
  ++
  (row addr |> rowIdentifier)
