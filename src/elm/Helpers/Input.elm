module Helpers.Input (Config, input) where

import Signal
import Json.Decode as Json exposing (..)
import Html exposing (input)
import Html.Attributes exposing (attribute, autofocus, value)
import Html.Events exposing (on, onFocus)

import Debug

type alias Config a = 
  { initialValue: String
  , autofocus: Bool
  , address: Signal.Address a
  , onFocus: a
  , onCommit: (String -> a)
  , onCancel: a
  }

input : Config a -> Html.Html
input config =
  Html.input 
    (
      [ Html.Attributes.value config.initialValue
      , autofocus config.autofocus
      , onFocus config.address config.onFocus
      ] 
      ++
      onFinish config.address config.onCommit config.onCancel
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