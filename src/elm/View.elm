module View (view) where 

import Html exposing (Html, div, span, text, table, thead, tbody, th, tr, td, button, input, textarea)
import Html.Attributes exposing (classList, value, key, id)
import Html.Events exposing (onClick, onDoubleClick, onFocus, onBlur)
import Signal
import Maybe

import Helpers
import Model exposing (Action(..), Mode(..))
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
        editing = case model.mode of
          Code -> Nothing
          Spreadsheet m -> if selected then m else Nothing
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
    div 
      []
      [ table
          [] 
          [ colHeader
          , body
          ]
      , textarea
          [ id "code" ]
          [ text model.code ]
      ]
    