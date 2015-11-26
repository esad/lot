module Helpers (cellInput) where

import Signal
import Json.Decode as Json exposing (..)
import Html exposing (input)
import Html.Attributes exposing (attribute, autofocus, value)
import Html.Events exposing (on)

import Debug

cellInput : String -> Signal.Address a -> (String -> a) -> a -> Html.Html
cellInput initialValue address commitAction cancelAction =
  input 
    (
      [ Html.Attributes.value initialValue
      , autofocus True
      ] ++
      onFinish address commitAction cancelAction
    )
    []

targetValueShouldCommit : Decoder (String, Bool)
targetValueShouldCommit =
  object2 (,)
      (at ["target", "value"] string)
      (oneOf [at ["target", "cancelOnBlur"] bool, succeed False])

onFinish : Signal.Address a -> (String -> a) -> a -> List Html.Attribute
onFinish address commitAction cancelAction =
  let
    handleBlur (value, cancel) =
      Signal.message address (if cancel then cancelAction else (commitAction value))
  in
    [ attribute "onkeydown"
        """
        if (event.keyCode == 13) {
          event.target.cancelOnBlur = false
          event.target.blur()
        } else if (event.keyCode == 27) {
          event.target.cancelOnBlur = true
          event.target.blur()
        }
        """
    , on "blur" targetValueShouldCommit handleBlur
    ]