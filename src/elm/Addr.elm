module Addr (Addr, row, col, fromColRow, add) where

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