module Helpers.Select (select) where

import Array
import String

import Html exposing (option, text)
import Html.Attributes exposing (attribute, selected, value)
import Html.Events exposing (on, onFocus)

import Json.Decode exposing (andThen, at, succeed, fail, string)

-- Produces a select box from list of values and their string representations
-- which upon selection, uses supplied onSelect to produce a message that is
-- then sent to the supplied address
select : List (b, String) -> b -> Signal.Address a -> (b -> a) -> Html.Html
select options selection address onSelect =
  let
    arrOptions = 
      Array.fromList options
    stringAsInt =
      Json.Decode.customDecoder string String.toInt
    decodeOption =
      at ["target", "value"] stringAsInt
      `andThen`
      \i ->
        case Array.get i arrOptions of
          Just (val, _) -> succeed val
          Nothing -> fail "Unknown option selected"
  in
  Html.select
    [on "input" decodeOption (\val -> onSelect val |> Signal.message address)]
    (List.indexedMap (\i (v,str) ->
      option ((if v == selection then [selected True] else []) ++ [value <| toString i]) [text str]
    ) options)