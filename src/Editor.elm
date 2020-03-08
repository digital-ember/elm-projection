module Editor exposing (..)

import Structure exposing (..)
import Html exposing (..)
import Html.Attributes as HtmlA


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

constantCell : String -> Node Cell
constantCell text =
         createNode ConstantCell
            |> addText "constant" text 
        

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
                [ text ("editor empty" ++ pathOf cell) ]
        
            Just content ->
                List.map viewContent content
    in
      div [] htmlContent
        
         
viewContent : Node Cell -> Html Msg
viewContent cell =
  div [] [
  case isaOf cell of
      RootCell ->
          renderStackCell cell Vert

      ConstantCell ->
          renderConstantCell cell

      InputCell ->
          renderInputCell cell

      StackCell orientation ->
          renderStackCell cell orientation
  ]

renderStackCell : Node Cell -> Orientation -> Html Msg
renderStackCell cell orientation =
    div []
        [ viewCell cell ]

renderConstantCell : Node Cell -> Html Msg
renderConstantCell cell =
    span [] [ text (propertyStringValueOf cell "constant" |> Maybe.withDefault "") ]


renderInputCell : Node Cell -> Html Msg
renderInputCell cell =
    div []
        [ input
            [ HtmlA.style "border-width" "0px"
             , HtmlA.style "border" "none"
             , HtmlA.style "outline" "none"
             --, HtmlA.map (\cellMsg -> PipeMsg cellMsg) (produceKeyboardMsg cell)
             , HtmlA.placeholder "<no value>"
             , HtmlA.value (propertyStringValueOf cell "input" |> Maybe.withDefault "")
             --, HtmlA.map (\cellMsg -> PipeMsg cellMsg) (HtmlE.onInput (Input cell))
             ]
            
            []
        ]
  