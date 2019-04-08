import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

import Cell exposing (..)
import Color as C

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
    , cellView Cell.genericCell
    ]


-- temp for testing
cellView : Cell -> Html msg
cellView inputCell =
  div []
    [ text ("Symbol: " ++ (String.fromChar inputCell.symbol))
    , text (" Symbol Color: " ++ (C.toCssString inputCell.symbolColor))
    , text (" Background Color: " ++ (C.toCssString inputCell.backgroundColor))
    ]