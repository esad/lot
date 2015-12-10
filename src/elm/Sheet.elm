module Sheet (Cell(..), Sheet, initialize, get, update, toList, cell2str, translate, insertRow, insertCol) where

import Addr exposing (Addr)
import Matrix
import Helpers.Matrix
import Basics

type Cell
  = TextCell String 
  | EmptyCell

cell2str : Cell -> Maybe String
cell2str cell =
  case cell of
    EmptyCell -> Nothing
    TextCell text -> Just text

type alias Sheet =
  { cells : Matrix.Matrix Cell }

loc : Addr -> Matrix.Location
loc addr = 
  (Addr.row addr, Addr.col addr)

initialize : Int -> Int -> Sheet 
initialize numCols numRows = 
  { cells = Matrix.matrix numRows numCols (always EmptyCell) }

rowCount : Sheet -> Int 
rowCount sheet = 
  Matrix.rowCount sheet.cells

colCount : Sheet -> Int 
colCount sheet = 
  Matrix.colCount sheet.cells

clamp : Addr -> Sheet -> Addr 
clamp addr sheet =
  Addr.fromColRow (Basics.clamp 0 (Matrix.colCount sheet.cells-1) (Addr.col addr)) (Basics.clamp 0 (Matrix.rowCount sheet.cells-1) (Addr.row addr))

get : Addr -> Sheet -> Maybe Cell
get addr sheet =
  Matrix.get (loc addr) sheet.cells

update : Addr -> (Cell -> Cell) -> Sheet -> Sheet
update addr f sheet =
  { sheet | cells = Matrix.update (loc addr) f sheet.cells }

-- Translates address by given number of cols and rows, clamping it if it exceeds the current sheet
translate : Addr -> Int -> Int -> Sheet -> Addr
translate src cols rows sheet =
  Addr.fromColRow cols rows
  |> Addr.add src
  |> flip clamp sheet

---- Returns list of rows, each a list containing cells for each column of that row
toList : Sheet -> List (List Cell)
toList sheet =
  Matrix.toList sheet.cells

insertRow : Int -> Sheet -> Sheet
insertRow row sheet =
  { sheet | cells = Helpers.Matrix.insertRow row (always EmptyCell) sheet.cells }

insertCol : Int -> Sheet -> Sheet
insertCol col sheet =
  { sheet | cells = Helpers.Matrix.insertCol col (always EmptyCell) sheet.cells }