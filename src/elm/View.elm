module View (view) where 

import Html exposing (Html, div, span, text, table, thead, tbody, th, tr, td, button, input, textarea, footer, main', ul, li)
import Html.Attributes exposing (class, classList, value, key, id)
import Html.Events exposing (onClick, onDoubleClick, onFocus, onBlur)
import Signal
import Maybe
import String

import Helpers
import Model exposing (Action(..))
import Sheet
import Cell exposing (Cell(..))
import Constraint exposing (Context(..))
import Addr

type Header = RowHeader | ColHeader

nbsp = "\xa0"

view : Signal.Address Action -> Model.Model -> Html
view address model =
  let
    viewTableau t =
      let
        globalConstraints =
          List.filterMap (\(src, c) -> if Constraint.hasContext GlobalContext c then Just src else Nothing) t
      in
        ul
          []
          (List.map (\src -> li [] [text src]) globalConstraints)
    viewCell addr cell =
      let
        css_classes =
          case cell of
            TextCell _ -> [("text", True)]
            EmptyCell -> []
            ResultCell _ -> [("value", True)]
        selected = addr == model.selection
        editing = if selected then model.editing else Nothing
      in 
        case editing of
          Nothing ->
            td
              [ key (toString addr)
              , classList <| ("selected", selected) :: css_classes
              , onDoubleClick address (Edit Nothing)
              , onClick address (Select addr)
              ]
              [
                cell
                |> Cell.toString
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
          ColHeader -> (Addr.col, Addr.colIdentifier >> String.toUpper, th)
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
    div [ classList [("app", True)] ]
      [ main' [] <|
        case model.solver of
          Nothing ->
            [ div [ class "loader" ] [text "Loading solver..."] ]
          Just (Err _) ->
            [ div [ class "loader" ] [text "Error loading solver. Check console output for possible hints."] ]
          Just (Ok _) ->
            [ 
              table []
                [ colHeader
                , body
                ]
            ]
      , div
          [ id "constraints" ]
          [ viewTableau model.tableau
          , footer [] [
              Helpers.cellInput "" address (\str -> AddGlobalConstraint str) Cancel
            ]
          ]
      ]
