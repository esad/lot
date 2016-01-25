module View (view) where 

import Html exposing (Html, a, div, span, text, table, thead, tbody, th, tr, td, button, textarea, footer, main', ul, li)
import Html.Attributes exposing (class, classList, value, key, id)
import Html.Events exposing (onClick, onDoubleClick, onFocus, onBlur)
import Signal
import Maybe
import String

import Helpers.Input
import Model exposing (Action(..), Focus(..))
import Sheet
import Cell exposing (Cell(..))
import Constraint exposing (Context(..))
import Addr
import Set

type Header = RowHeader | ColHeader

nbsp = "\xa0"

view : Signal.Address Action -> Model.Model -> Html
view address model =
  let
    selectedCellIdentifier =
      Addr.toIdentifier model.selection
    viewTableau t =
      ul
        []
        (List.indexedMap (\i c -> 
          li
            [ classList [("related", Constraint.identifiers c |> Set.member selectedCellIdentifier)] ] 
            [ text <| Constraint.toString GlobalContext c
            , a [onClick address (DropConstraint i)] [ text "✕"] 
            ]
        ) model.tableau)
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
          Just editInfo ->
            td
              [ key (toString addr)
              , classList [("selected", selected), ("editing", True)]
              ]
              [
                Helpers.Input.input
                  { initialValue = editInfo.initialValue
                  , autofocus = True
                  , address = address
                  , onFocus = Nop
                  , onCommit = Commit addr
                  , onCancel = Cancel
                  }
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
    div
      [ classList [("app", True), ("unsat", model.unsat)] ]
      (case model.solver of
      Nothing ->
        [ div [ class "loader" ] [text "Loading solver..."] ]
      Just (Err _) ->
        [ div [ class "loader" ] [text "Error loading solver. Check console output for possible hints."] ]
      Just (Ok _) ->
        [ main' 
          [ classList [("selected", model.focus == Sheet)]
          , onClick address (SwitchFocus Sheet)
          ]
          [ 
            table []
              [ colHeader
              , body
              ]
          ]
        , div
            [ id "unsat" ]
            [ text "Constraints cannot be satisfied" 
            , a [onClick address Undo] [ text "Undo"]
            ]
        , div
            [ id "constraints"
            , classList [("selected", model.focus == Tableau)]
            --, onClick address (SwitchFocus Tableau)
            ]
            [ viewTableau model.tableau
            , footer [] [
                Helpers.Input.input 
                  { initialValue = ""
                  , autofocus = False
                  , address = address
                  , onFocus = SwitchFocus Tableau
                  , onCommit = AddConstraint
                  , onCancel = SwitchFocus Sheet
                  }
              ]
            ]
        ])
