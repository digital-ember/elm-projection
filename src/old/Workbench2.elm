module Workbench2 exposing(..)


import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Html exposing(..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Json.Decode as JsonD
import Set
import Task

-- MODEL

type alias Runtime =
  { mouse : Mouse
  , keyboard : Keyboard
--  , time : Time --not sure if needed, certainly not at the beginning
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

type Tool model =
  Tool model Runtime

type Msg
  = KeyChanged Bool String
  | MouseMove Float Float
  | MouseClick
  | MouseButton Bool
  --| UpdateProperty (a -> String) a String
  
initialRuntime : Runtime
initialRuntime =
  { mouse = Mouse 0 0 False False
  , keyboard = emptyKeyboard
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

type Cell =
  Cell
    { id : String
    , mbParent : Maybe Cell 
    }
    KCell

type KCell
  = ConstantCell String
  | PropertyCell String
  | StackCell StackOrientation (List Cell)

type StackOrientation 
  = Horiz
  | Vert


constCell : String -> Maybe Cell -> Cell
constCell txt mbParent =
  Cell { id = "", mbParent = mbParent } (ConstantCell txt)

propertyCell : String -> Maybe Cell -> Cell
propertyCell txt mbParent =
  Cell { id = "", mbParent = mbParent } (PropertyCell txt)

verticalStack : Maybe Cell -> (a -> Cell) -> List a -> Cell
verticalStack mbParent toCell things =
  let
    cells = List.map toCell things
    stack = Cell { id = "", mbParent = mbParent } (StackCell Vert cells)
  in
    stack



tool : (Runtime -> model -> Cell) -> (Runtime -> model -> model) -> model -> Program () (Tool model) Msg
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

render : Cell -> Html Msg
render (Cell data kcell)  =
  case kcell of  
    ConstantCell txt -> 
      renderConstantCell txt
    
    PropertyCell txt -> 
      renderPropertyCell txt

    StackCell orientation cells -> 
      renderStackCell orientation cells

renderStackCell orientation cells =
  div 
    [ style "margin" "0"
    ]
    [ div 
      [ style "margin" "3px 20px"]
      <| List.map render cells
    ]

renderPropertyCell txt =
  input 
    [ placeholder "<no value>"
    , value txt
    --, id event.id
    --, onInput (UpdateEvent event)
    --, produceKeyboardMsg event
    ]
    []

renderConstantCell txt =
  span [] [ text txt ]


toolUpdate : (Runtime -> model -> model) -> Msg -> Tool model -> Tool model
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

    --UpdateProperty access a txt ->
      --Tool (updateModel runtime model) runtime 



toolSubscriptions : Sub Msg
toolSubscriptions =
  Sub.batch
    [ Events.onKeyUp (JsonD.map (KeyChanged False) (JsonD.field "key" JsonD.string))
    , Events.onKeyDown (JsonD.map (KeyChanged True) (JsonD.field "key" JsonD.string))
    , Events.onClick (JsonD.succeed MouseClick)
    , Events.onMouseDown (JsonD.succeed (MouseButton True))
    , Events.onMouseUp (JsonD.succeed (MouseButton False))
    , Events.onMouseMove (JsonD.map2 MouseMove (JsonD.field "pageX" JsonD.float) (JsonD.field "pageY" JsonD.float))
    ]



