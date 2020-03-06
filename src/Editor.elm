module Editor exposing(..)

import Structure exposing(..)
import Html exposing (..)


type Msg  
  = NoOp

rootCell : List Property -> List Feature -> Node 
rootCell properties features =
    createNode "root" 
      |> addProperties properties
      |> addFeatures features 
      |> addPaths


vertStackCell : List Property -> List Feature -> Node
vertStackCell properties features =
    createNode "vStack" 
        |> addProperties properties
        |> addFeatures features


inputCell : List Property -> String -> Node
inputCell properties text = 
    createNode "input" 
        |> addProperties properties
        |> addText "input" text 


constantCell : List Property -> String -> Node
constantCell properties text =
    createNode "constant" 
        |> addProperties properties
        |> addText "constant" text 



-- EDITOR 

viewCell : Node -> Html Msg
viewCell cell =
    Debug.todo "todo"
    