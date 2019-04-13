import Browser
import Html exposing (Html, button, div, text, td, tr, table)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)

import Cell as Ce
import Color as Co
import Grid as G
import Array as A

main =
  Browser.sandbox { init = 0, update = update, view = view }

type Msg = Increment | Decrement

update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    , cellView Ce.genericCell
    ]


-- temp for testing
cellView : Ce.Cell -> Html msg
cellView inputCell =
  div []
    [ text ("Symbol: " ++ (String.fromChar inputCell.symbol))
    , text (" Symbol Color: " ++ (Co.toCssString inputCell.symbolColor))
    , text (" Background Color: " ++ (Co.toCssString inputCell.backgroundColor))
    , gridDisplay testHello
    ]

cellDisplay : Ce.Cell -> Html msg
cellDisplay inputCell =
  td
  [ style "width" "20px"
  , style "height" "20px"
  , style "text-align" "center"
  , style "font-family" "monospace"
  , style "background-color" (Co.toCssString inputCell.backgroundColor)
  , style "color" (Co.toCssString inputCell.symbolColor)
  ]
  [ text (String.fromChar inputCell.symbol) ]

gridDisplay : G.Grid -> Html msg
gridDisplay inGrid =
  let
    rowList = A.toList (A.map A.toList (G.rows inGrid))
    rowToTr inRow = tr [] (List.map cellDisplay inRow)
    rowsAsHtml = List.map rowToTr rowList
  in
    table [] rowsAsHtml

testHello : G.Grid
testHello =
  let
    genericGrid = 
      { width = 1
      , height = 1
      , cells = A.fromList [Ce.genericCell]
      }
    stack = 
      Maybe.withDefault
        genericGrid
        (G.combineVert G.helloGrid G.helloGrid)
  in
    Maybe.withDefault
      genericGrid
      (G.combineHor stack stack)
