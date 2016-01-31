module Sheet (Sheet, initialize, get, fold, map, update, toList, move, insertRow, insertCol, decoder, encode) where

import Cell exposing (Cell, Cell(..))
import Addr exposing (Addr, Direction(..))
import Matrix
import Helpers.Matrix
import Basics

import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode

type alias Sheet =
  { cells : Matrix.Matrix Cell }

decoder : Decode.Decoder Sheet
decoder =
  Decode.object1
    (\list -> { cells = Matrix.fromList list })
    (Decode.list (Decode.list Cell.decoder))

encode : Sheet -> Encode.Value
encode sheet =
  sheet
  |> toList
  |> List.map (\row ->
      row
      |> List.map Cell.encode
      |> Encode.list
  )
  |> Encode.list

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

fold : ((Addr,Cell) -> a -> a) -> a -> Sheet -> a
fold f init sheet =
  sheet
  |> toList
  |> List.indexedMap (\row cells -> List.indexedMap (\col cell -> (Addr.fromColRow col row, cell)) cells)
  |> List.concat -- List (Addr, Cell)
  |> List.foldl f init

map : (Addr -> Cell -> Cell) -> Sheet -> Sheet
map f sheet =
  { sheet
    | cells = 
      Matrix.mapWithLocation (\loc cell ->
        f (Addr.fromColRow (Matrix.col loc) (Matrix.row loc)) cell
      ) sheet.cells
  }

update : Addr -> (Cell -> Cell) -> Sheet -> Sheet
update addr f sheet =
  { sheet | cells = Matrix.update (loc addr) f sheet.cells }

-- Translates address by given direction, clamping it if it exceeds the current sheet
move : Addr -> Addr.Direction -> Sheet -> Addr
move a dir sheet =
  let
    end =
      case dir of
        Left  -> Addr.fromColRow (Addr.col a - 1) (Addr.row a)
        Right -> Addr.fromColRow (Addr.col a + 1) (Addr.row a)
        Up    -> Addr.fromColRow (Addr.col a) (Addr.row a - 1)
        Down  -> Addr.fromColRow (Addr.col a) (Addr.row a + 1)
  in
    clamp end sheet

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