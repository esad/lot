module View (view) where 

import Html exposing (Html, span, text, table, tr, td, button, input)
import Html.Attributes exposing (classList, value, key)
import Html.Events exposing (onClick, onDoubleClick)
import Signal

import Helpers
import Model exposing (Action(..))
import Sheet
import Addr

view : Signal.Address Action -> Model.Model -> Html
view address model =
  let
    viewCell addr cell =
      let
        selected = addr == model.selection
        editing = if selected then model.editing else Nothing
      in 
        case editing of
          Nothing ->
            td
              [ key (toString addr)
              , classList [("selected", selected)]
              , onDoubleClick address (Edit Nothing)
              , onClick address (Select addr)
              ]
              [
                Sheet.cell2str cell
                  |> Maybe.withDefault "\xa0" -- &nbsp;
                  |> text
              ]
          Just editStr ->
            td
              [ key (toString addr)
              , classList [("selected", selected), ("editing", True)]
              ]
              [
                Helpers.cellInput editStr address (\str -> Commit addr str) Cancel
              ]
    viewRow row cols =
      cols
        |> List.indexedMap (\col cell -> viewCell (Addr.fromColRow col row) cell)
        |> tr []
  in
    model.sheet
      |> Sheet.toList
      |> List.indexedMap viewRow
      |> table []