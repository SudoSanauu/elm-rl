module Cell exposing
    ( Cell, genericCell
    , setSymbol, setSymbolColor, setBackgroundColor
    )

import Color as C exposing (Color) 


type alias Cell =
    { symbol : Char
    , symbolColor : Color
    , backgroundColor : Color
    }


genericCell : Cell
genericCell =
    { symbol = '.'
    , symbolColor = C.white
    , backgroundColor = C.black
    }

setSymbol : Char -> Cell -> Cell
setSymbol newChar inCell =
    { inCell | symbol = newChar }


setSymbolColor : Color -> Cell -> Cell
setSymbolColor newColor inCell =
    { inCell | symbolColor = newColor }

setBackgroundColor : Color -> Cell -> Cell
setBackgroundColor newColor inCell =
    { inCell | backgroundColor = newColor }