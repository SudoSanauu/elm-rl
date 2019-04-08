module Cell exposing (..) 
-- at end only expose what needs exposing
import Color as C


type alias Cell =
  { symbol : Char
  , symbolColor : C.Color
  , backgroundColor : C.Color
  }