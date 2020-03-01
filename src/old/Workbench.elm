module Workbench exposing(..)


import Browser
import Browser.Events as Events
import Html exposing(..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Json.Decode as JsonD
import Set



type alias Runtime =
  { mouse : Mouse
  , keyboard : Keyboard
--  , time : Time --not sure if needed, certainly not at the beginning
  }

initialRuntime : Runtime
initialRuntime =
  { mouse = Mouse 0 0 False False
  , keyboard = emptyKeyboard
  }

type alias Mouse =
  { x : Number
  , y : Number
  , down : Bool
  , click : Bool
  }

type alias Number = Float

type alias Keyboard =
  { up : Bool
  , down : Bool
  , left : Bool
  , right : Bool
  , space : Bool
  , enter : Bool
  , shift : Bool
  , backspace : Bool
  , keys : Set.Set String
  }

emptyKeyboard : Keyboard
emptyKeyboard =
  { up = False
  , down = False
  , left = False
  , right = False
  , space = False
  , enter = False
  , shift = False
  , backspace = False
  , keys = Set.empty
  }


type Tool model =
  Tool model Runtime

type alias Cell a =
  Html (Msg a)

{--
type Cell parent child =
  Cell
  -- encode commonalities of cells here
    String -- id
    (KCell parent child)

type KCell parent child
  = HorizCellCollection parent (List (Cell parent child))
  | VertCellCollection parent (List (Cell parent child))
  | ConstantCell parent String
  | PropertyCell parent String
  | RefCell parent (Cell parent child)


horizCellCollection : parent -> List (Cell parent child) -> Cell parent child
horizCellCollection parent cells =
  Cell "" (HorizCellCollection parent cells)

vertCellCollection : parent -> List (Cell parent child) -> Cell parent child
vertCellCollection parent cells =
  Cell "" (VertCellCollection parent cells)

constantCell node text =
  Cell "" (ConstantCell node text)

propertyCell node text =
  Cell "" (PropertyCell node text)

refCell node cell =
  Cell "" (RefCell node cell)
--}

constantCell : String -> Cell a
constantCell txt =
  span [] [ text txt ]

propertyCell : (a -> String) -> a -> Cell a
propertyCell access a =
  div 
    [ ]
    [ input 
        [ placeholder "<no value>"
        , value (access a)
        , onInput (UpdateProperty access a)
        --, id event.id
        --, produceKeyboardMsg event
        ]
        []
    ]

vertColl : (a -> Cell a) -> List a -> Cell a
vertColl viewElement elements =
  div 
    [ style "margin" "0"
    ]
    [ div 
      [ style "margin" "3px 20px"]
      <| List.map viewElement elements
    ]

makeVertColl : List (Cell a) -> (Cell a)
makeVertColl cells = 
  div [ ] cells


type Msg a
  = KeyChanged Bool String
  | MouseMove Float Float
  | MouseClick
  | MouseButton Bool
  | UpdateProperty (a -> String) a String



tool : (Runtime -> model -> Cell a) -> (Runtime -> model -> model) -> model -> Program () (Tool model) (Msg a)
tool editor updateModel initialModel =
  let
    init () =
      ( Tool initialModel initialRuntime
      , Cmd.none
      )

    view (Tool model runtime) =
      { title = "Workbench"
      , body = [ render (editor runtime model) ]
      }

    update msg toolModel =
      ( toolUpdate updateModel msg toolModel
      , Cmd.none
      )

    subscriptions (Tool _ _) =
      toolSubscriptions
  in
    Browser.document
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }  

render : Cell a -> Html (Msg a)
render cell  =
  div [] [ cell ]
  

{--
renderCell : Cell parent child -> Html Msg
renderCell (Cell id kcell) =
  case kcell of  
    ConstantCell parent txt -> 
      renderConstantCell txt
    
    PropertyCell parent txt -> 
      renderPropertyCell parent txt

    RefCell parent cell -> 
      renderRefCell parent cell

    VertCellCollection parent cells -> 
      div [] [ text "VertColl Todo" ]

    HorizCellCollection parent cells -> 
      div [] [ text "HorizColl Todo" ]


renderRefCell parent cell =
  renderCell cell

renderPropertyCell parent txt =
  input 
    [ placeholder "<no value>"
    , value txt
    --, id event.id
    --, onInput (UpdateEvent event)
    --, produceKeyboardMsg event
    ]
    []

renderConstantCell txt =
  text txt
--}

toolUpdate : (Runtime -> model -> model) -> Msg a -> Tool model -> Tool model
toolUpdate updateModel msg (Tool model runtime) =
  case msg of
    KeyChanged _ _ -> 
      Tool model runtime

    MouseMove _ _ -> 
      Tool model runtime

    MouseClick ->
      Tool model runtime

    MouseButton _ -> 
      Tool model runtime

    UpdateProperty access a txt ->
      Tool (updateModel runtime model) runtime 



toolSubscriptions : Sub (Msg a)
toolSubscriptions =
  Sub.batch
    [ Events.onKeyUp (JsonD.map (KeyChanged False) (JsonD.field "key" JsonD.string))
    , Events.onKeyDown (JsonD.map (KeyChanged True) (JsonD.field "key" JsonD.string))
    , Events.onClick (JsonD.succeed MouseClick)
    , Events.onMouseDown (JsonD.succeed (MouseButton True))
    , Events.onMouseUp (JsonD.succeed (MouseButton False))
    , Events.onMouseMove (JsonD.map2 MouseMove (JsonD.field "pageX" JsonD.float) (JsonD.field "pageY" JsonD.float))
    ]


