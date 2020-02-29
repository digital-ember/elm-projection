module Node exposing(..)

import Browser exposing(sandbox)
import Html exposing (Html, div)

type Node isa =
  Node 
    { isa : isa
    , traits : List (Trait isa)
    }

type alias Trait isa =
  { name : String
  , children : List (Node isa)
  }

isa : Node isa -> isa
isa ( Node data )  =
  data.isa


traitsOf : Node isa -> List (Trait isa)
traitsOf ( Node data )  =
  data.traits

ndata : Node isa -> { isa : isa, traits : List (Trait isa) }
ndata ( Node data ) =
  data

children : Node isa -> Trait isa -> List (Node isa)
children  ( Node { traits } ) trait =
  let 
    mbTrait = List.filter (\t -> t == trait) traits |> List.head
  in
    case mbTrait of
      Just t ->
        t.children  
    
      Nothing ->
        [ ]
            

type Nid nid =
  Nid nid


nid : Nid nid -> nid
nid ( Nid n ) =
  n

type alias Selection nid =
  { index : Int
  , nid : Nid nid 
  }

    
type Model isaI isaO =
  Model 
    { root : Node isaI
    , asString : Node isaI -> String
    , transform : Node isaI -> Node isaO 
    , selection : Maybe (Selection isaO)
    }


init : Node isaI -> (Node isaI -> Node isaO) -> Model isaI isaO
init root transform =
  Model 
    { root = root 
    , asString = \_ -> ""
    , transform = transform
    , selection = Nothing
    }

type MsgI isa
  = Select (Nid isa)

type Msg isaI isaO
  = Internal (MsgI isaI)
  | External isaO

updateRoot : (isaI -> Bool) -> (isaI -> isaI) -> Model isaI isaO -> Model isaI isaO
updateRoot selector updater (Model m) =
  let
    rootNew =
      updateNode selector updater m.root
  in
    Model { m | root = rootNew }

updateNode : (isa -> Bool) -> (isa -> isa) -> Node isa -> Node isa
updateNode selector updater node =
  let
    traitsNew =
      List.map (\t -> { t | children = List.map (updateNode selector updater) t.children }) (traitsOf node)

    isaCurrent = isa node
    isaNew =
      if selector isaCurrent then
        updater isaCurrent
      else
        isaCurrent
  in
    Node
      { isa = isaNew
      , traits = traitsNew
      }

{-- wonder if it is necessary like this, since only internal messages are handled and external messages do nothing to the model --}
update : Msg isaI isaO -> Model isaI isaO -> Model isaI isaO
update msg model =
  case msg of
    Internal msgI ->
      updateI msgI model

    External _ -> 
      model

updateI : MsgI isaI -> Model isaI isaO -> Model isaI isaO
updateI msg model =
  case msg of
      Select _ ->
        -- set selection
        model
          


{--
getSelected : Model isaI isaO -> Maybe (Node isaI)
getSelected model =
  Maybe.map .index model.selection
    |> Maybe.andThen (\index -> LX.getAt index guts.visibleAnnotatedNodes) -- XXXXXXX
--}

view : Model isaI isaO -> Html (Msg isaI isaO)
view model =
  div [] [ ]

chain : Model isaI isaO -> Node isaO
chain (Model m) =
  m.transform m.root



pipe : Node isaI -> (Node isaI -> Node isaO) -> Program () (Model isaI isaO) (Msg isaI isaO)
pipe nodeI transform updateIsa =
  Browser.sandbox 
    { init = init nodeI transform
    , update = update
    , view = view 
    }
  