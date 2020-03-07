module Editor exposing (..)

import Structure exposing (..)
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


createRootCell : Node Cell
createRootCell =
    createRoot RootCell

constantCell : String -> Node Cell -> Node Cell
constantCell text parent =
    parent 
      |> addToDefault 
        ( createNode ConstantCell
            |> addText "constant" text 
        )

inputCell : String -> Node Cell
inputCell text =
    createNode InputCell
        |> addText "input" text
    

vertStackCell : Node Cell -> Node Cell
vertStackCell parent =
    parent 
      |> addToDefault 
          ( createNode (StackCell Vert) )

range : List (Node Cell) -> Node Cell -> Node Cell
range children parent =
    parent 
      |> addToDefaultRange children


-- EDITOR


viewCell : Node Cell -> Html Msg
viewCell cell =
    let
        htmlContent = 
          case getUnderDefault cell of
            Nothing ->
                [ text "editor empty" ]
        
            Just content ->
                List.map viewContent content
    in
      div [] htmlContent
        
         


viewContent : Node Cell -> Html Msg
viewContent cell =
  case isaOf cell of
      RootCell ->
          text "rootCell"
  
      (StackCell orientation) ->
          text "stackCell"


      ConstantCell ->

          text "constantCell"


      InputCell -> 

          text "inputCell"

          
  