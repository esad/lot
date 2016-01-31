module Cell (Cell, Cell(..), toString, decoder, encode) where

import String
import Maybe

import Json.Decode as Decode
import Json.Encode as Encode

type Cell
  = EmptyCell
  | TextCell String
  | ResultCell (Maybe Float)
  

decoder : Decode.Decoder Cell
decoder =
  Decode.oneOf
    [ Decode.null EmptyCell
    , Decode.object1 TextCell Decode.string
    , Decode.object1 (\f -> ResultCell (Just f)) Decode.float
    ]

encode : Cell -> Encode.Value
encode cell =
  case cell of
    EmptyCell -> Encode.null
    TextCell s -> Encode.string s
    ResultCell (Just f) -> Encode.float f
    ResultCell Nothing -> Encode.null

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