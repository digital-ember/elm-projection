module Pipe exposing(..)

import Browser
import Html exposing(div, span, text, input, Html)
import Html.Events exposing(onInput, onClick)
import Html.Attributes exposing(..)

type alias Pipe root appMsg =
  { root : root
  , xform : (root -> Cell appMsg)
  }


type Msg appMsg
  = PipeMsg (CellMsg appMsg)
  
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
    PipeMsg ( Input cell text ) ->
      pipe --this is where the backmapping to the actual app model mus thappen somehow: via dict lookup? just by passing on a appMsg? 

    PipeMsg (AppMsg appMsg) ->
      {pipe | root = appUpdate appMsg pipe.root }
        
  

render : Pipe root appMsg -> Html (Msg appMsg)
render pipe =
  let
    cellRoot = pipe.xform pipe.root
  in
    renderCell { localIndex = 0, id= ""} cellRoot
  
  

-- CELL DOMAIN

type Cell appMsg =
  Cell
    (List (CellAttribute (CellMsg appMsg) ))
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

type CellAttribute msg
  = CellAttribute (Html.Attribute msg) 

type alias CellIdAggregator = 
  { localIndex : Int
  , id : String 
  }


constantCell : List (CellAttribute (CellMsg appMsg)) -> String -> Cell appMsg
constantCell attributes txt =
  Cell attributes (ConstantCell txt)


propertyCell : List (CellAttribute (CellMsg appMsg)) -> String -> Cell appMsg
propertyCell attributes txt =
  Cell attributes (PropertyCell txt)


vertStackCell : List (CellAttribute (CellMsg appMsg)) -> List (Cell appMsg) -> Cell appMsg
vertStackCell attributes cells =
  Cell attributes (StackCell Vert cells)


renderCell : CellIdAggregator -> Cell appMsg -> Html (Msg appMsg)
renderCell { localIndex, id } cell  =
  let
    (Cell attributes kcell) = cell 
    cida = { localIndex = localIndex, id = id ++ String.fromInt localIndex }
  in
    case kcell of  
      ConstantCell txt -> 
        renderConstantCell cida attributes cell txt
      
      PropertyCell txt -> 
        renderPropertyCell cida attributes cell txt

      StackCell orientation cells -> 
        renderStackCell cida attributes orientation cells


renderConstantCell : CellIdAggregator -> List (CellAttribute (CellMsg appMsg)) -> Cell appMsg -> String -> Html (Msg appMsg)
renderConstantCell {id} attributes cell txt =
  span ( Html.Attributes.id id :: liftAttr attributes) [ text txt ]


renderPropertyCell : CellIdAggregator -> List (CellAttribute (CellMsg appMsg)) -> Cell appMsg -> String -> Html (Msg appMsg)
renderPropertyCell {id} attributes cell txt =
  div []
  [
    input 
      ([ Html.Attributes.id id 
      , placeholder "<no value>"
      , value txt
      , Html.Attributes.map (\cellMsg -> PipeMsg cellMsg) (onInput (Input cell))
      ] ++ liftAttr attributes)
      []
  ]


renderStackCell : CellIdAggregator -> List (CellAttribute (CellMsg appMsg)) -> StackOrientation -> List (Cell appMsg) -> Html (Msg appMsg)
renderStackCell cida attributes orientation cells =
  div 
    ( style "margin" "0"  :: liftAttr attributes )
    [ div 
      [ style "margin" "3px 20px"
      , Html.Attributes.id cida.id
      ]
      <| List.indexedMap (\i c -> renderCell { cida | localIndex = i } c) cells
    ]



liftAttr : List (CellAttribute (CellMsg appMsg)) -> List (Html.Attribute (Msg appMsg))
liftAttr attributes =
  List.map (\(CellAttribute htmlAttr) -> htmlAttr) attributes 
    |> List.map (Html.Attributes.map (\cellMsg -> PipeMsg cellMsg)) 

-- CELL ATTRIBUTES

cid : String -> CellAttribute msg
cid idParam =
  CellAttribute (id idParam)

onCellClick : msg -> CellAttribute msg
onCellClick msg =
  CellAttribute (onClick msg)
