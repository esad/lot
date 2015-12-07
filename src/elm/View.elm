module View (view) where 

import Html exposing (Html, span, text, table, thead, tbody, th, tr, td, button, input)
import Html.Attributes exposing (classList, value, key)
import Html.Events exposing (onClick, onDoubleClick)
import Signal
import Maybe

import Helpers
import Model exposing (Action(..))
import Sheet
import Addr

type Header = RowHeader | ColHeader

nbsp = "\xa0" 

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
                  |> Maybe.withDefault nbsp
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
    viewHeader header idx =
      let
        (component, identifier, tag) = case header of
          RowHeader -> (Addr.row, Addr.rowIdentifier, td)
          ColHeader -> (Addr.col, Addr.colIdentifier, th)
        selected = (component model.selection == idx)
      in
        tag
          [ classList [("selected", selected)]]
          [ 
            identifier idx
            |> text
          ]
    viewRow row cols =
      cols
        |> List.indexedMap (\col cell -> viewCell (Addr.fromColRow col row) cell)
        |> (::) (viewHeader RowHeader row)
        |> tr []
    sheet =
      Sheet.toList model.sheet
    colHeader = 
      sheet
      |> List.head 
      |> Maybe.withDefault []
      |> List.indexedMap (\col _ -> viewHeader ColHeader col)
      |> (::) (th [] [text nbsp]) -- corner
      |> tr []
    body =
      sheet
      |> List.indexedMap viewRow
      |> tbody []
  in
    table [] 
      [ colHeader
      , body
      ]
