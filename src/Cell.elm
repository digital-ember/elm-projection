module Cell exposing(..)

import Html exposing (..)
import Html.Attributes exposing (..)


verticalCollection : (a -> Html b) -> List a -> Html b
verticalCollection viewElement elements =
  div 
    [ style "margin" "0"
    ]
    [ div 
      [ style "margin" "3px 20px"]
      <| List.map viewElement elements
    ]

constant : List (Attribute msg) -> String -> Html msg
constant attributes str =
  span attributes [ text str ]