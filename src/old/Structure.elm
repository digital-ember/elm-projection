module Structure exposing(..)

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

type Model concept domainMsg =
  Model 
   { concept : concept 
   , domainMsg : domainMsg 
   }