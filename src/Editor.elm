module Editor exposing(..)

import Structure exposing(..)
import Html exposing (..)

type Cell 
  = RootCell
  | StackCell Orientation
  | ConstantCell
  | InputCell

type Orientation 
  = Vert
  | Horiz

type Msg  
  = NoOp

rootCell : List Property -> List Feature -> Node Cell
rootCell properties features =
    createNode RootCell 
      |> addProperties properties
      |> addFeatures features 
      |> addPaths


vertStackCell : List Property -> List Feature -> Node Cell
vertStackCell properties features =
    createNode (StackCell Vert) 
        |> addProperties properties
        |> addFeatures features
 

inputCell : List Property -> String -> Node Cell
inputCell properties text = 
    createNode InputCell 
        |> addProperties properties
        |> addText "input" text 


constantCell : List Property -> String -> Node Cell
constantCell properties text =
    createNode ConstantCell 
        |> addProperties properties
        |> addText "constant" text 



-- EDITOR 

viewCell : Node Cell -> Html Msg
viewCell cell =
    Debug.todo "todo"
    