module Pipe exposing(..)

import Browser
import Html exposing(div, span, text, input, Html)
import Html.Events exposing(onInput)
import Html.Attributes exposing(..)

type alias Pipe root appMsg =
  { root : root
  , xform : (root -> Cell appMsg)
  }


type Msg appMsg
  = NoOp
  | Behavior (CellMsg appMsg)
  
pipeProgram : root -> (root -> Cell appMsg) -> (appMsg -> root -> root) -> Program () (Pipe root appMsg) (Msg appMsg)
pipeProgram root xform appUpdate = 
  let
    init = { root = root, xform = xform } 

    update msg pipe =
      pipeUpdate appUpdate msg pipe

    view pipe =
      render pipe

  in  
    Browser.sandbox 
      { init = init
      , update = update
      , view = view
      }


pipeUpdate : (appMsg -> root -> root) -> (Msg appMsg) -> Pipe root appMsg -> Pipe root appMsg
pipeUpdate appUpdate msg pipe = 
  case msg of
    NoOp ->
      pipe  

    Behavior ( Input _ _ ) ->
      pipe --this is where the backmapping to the actual app model mus thappen somehow: via dict lookup? just by passing on a appMsg? 

    Behavior (AppMsg appMsg) ->
      {pipe | root = appUpdate appMsg pipe.root }
        
  

render : Pipe root appMsg -> Html (Msg appMsg)
render pipe =
  let
    cellRoot = pipe.xform pipe.root
  in
    renderCell cellRoot
  
  

-- CELL DOMAIN

type Cell appMsg =
  Cell
    (List (Html.Attribute (CellMsg appMsg) ))
    (KCell appMsg)

type KCell appMsg
  = ConstantCell String
  | PropertyCell String
  | StackCell StackOrientation (List (Cell appMsg))

type StackOrientation 
  = Horiz
  | Vert

type CellMsg appMsg
  = Input (Cell appMsg) String
  | AppMsg appMsg


constantCell : List (Html.Attribute (CellMsg appMsg)) -> String -> Cell appMsg
constantCell attributes txt =
  Cell attributes (ConstantCell txt)


propertyCell : List (Html.Attribute (CellMsg appMsg)) -> String -> Cell appMsg
propertyCell attributes txt =
  Cell attributes (PropertyCell txt)

vertStackCell : List (Html.Attribute (CellMsg appMsg)) -> List (Cell appMsg) -> Cell appMsg
vertStackCell attributes cells =
  Cell attributes (StackCell Vert cells)


renderCell : Cell appMsg -> Html (Msg appMsg)
renderCell cell  =
  case cell of  
    (Cell attributes (ConstantCell txt)) -> 
      renderConstantCell attributes cell txt
    
    (Cell attributes (PropertyCell txt)) -> 
      renderPropertyCell attributes cell txt

    (Cell attributes (StackCell orientation cells)) -> 
      renderStackCell attributes orientation cells

renderStackCell : List (Html.Attribute (CellMsg appMsg)) -> StackOrientation -> List (Cell appMsg) -> Html (Msg appMsg)
renderStackCell attributes orientation cells =
  div 
    ( style "margin" "0"  :: liftAttr attributes )
    [ div 
      [ style "margin" "3px 20px"]
      <| List.map renderCell cells
    ]

renderPropertyCell : List (Html.Attribute (CellMsg appMsg)) -> Cell appMsg -> String -> Html (Msg appMsg)
renderPropertyCell attributes cell txt =
  div []
  [
    input 
      ([ placeholder "<no value>"
      , value txt
      , Html.Attributes.map (\cellMsg -> Behavior cellMsg) (onInput (Input cell))
      ] ++ liftAttr attributes)
      []
  ]

renderConstantCell : List (Html.Attribute (CellMsg appMsg)) -> Cell appMsg -> String -> Html (Msg appMsg)
renderConstantCell attributes cell txt =
  span ([ ] ++ liftAttr attributes) [ text txt ]


liftAttr : List (Html.Attribute (CellMsg appMsg)) -> List (Html.Attribute (Msg appMsg))
liftAttr attributes =
  List.map (Html.Attributes.map (\cellMsg -> Behavior cellMsg)) attributes 