module Cell (Cell, Cell(..), toString) where

type Cell
  = TextCell String 
  | EmptyCell

toString : Cell -> Maybe String
toString cell =
  case cell of
    EmptyCell -> Nothing
    TextCell text -> Just text