module CasedString exposing(..)

import Html exposing(Html, text, span, div)
import Node as N
import Browser exposing(..)


type alias Domain =
  { text : String 
  , isUpperCase : Bool 
  }

{--type alias Model =
   N.Model Text (Html Msg)
--}

type Msg
  = NodeMsg (N.Msg String (Html Msg))
  | ToggleCasing

main =
  N.pipe 
    ( N.Node { isa = Domain "hello" False, traits = [ ] } )
    transform


transform : N.Node Domain -> N.Node (Html Msg) 
transform (N.Node ndata) =
  N.Node { isa = span [] [ text ndata.isa.text ], traits = [] }

