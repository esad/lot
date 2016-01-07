module Identifier (Identifier, fromString, toString) where

import String

type Identifier = Identifier String

fromString : String -> Identifier
fromString str =
  Identifier <| String.toLower str

toString : Identifier -> String
toString (Identifier s) =
  s