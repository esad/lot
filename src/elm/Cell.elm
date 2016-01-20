module Cell (Cell, Cell(..), toString) where

import String
import Maybe

type Cell
  = EmptyCell
  | TextCell String
  | ResultCell (Maybe Float)
  
toString : Cell -> Maybe String
toString cell =
  case cell of
    EmptyCell ->
      Nothing
    TextCell text ->
      Just text
    ResultCell (Just i) ->
      Just <| Basics.toString i
    ResultCell Nothing ->
      Just "..."