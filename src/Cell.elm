module Cell exposing (Cell, genericCell)

import Color as C


type alias Cell =
    { symbol : Char
    , symbolColor : C.Color
    , backgroundColor : C.Color
    }


genericCell : Cell
genericCell =
    { symbol = '.'
    , symbolColor = C.white
    , backgroundColor = C.black
    }
