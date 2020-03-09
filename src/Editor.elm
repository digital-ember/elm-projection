module Editor exposing (..)

import Structure exposing (..)
import Html exposing (..)
import Html.Attributes as HtmlA
import Html.Events as HtmlE


type Cell
    = RootCell
    | StackCell Orientation
    | ConstantCell
    | InputCell
    | PlaceholderCell


type Orientation
    = Vert
    | Horiz


type Msg
    = NoOp
    | Swallow String
    | ReplacePlaceHolder String


createRootCell : Node Cell
createRootCell =
    createRoot RootCell


with : Node Cell -> Node Cell -> Node Cell
with node =
    addToDefault node


withRange : List (Node Cell) -> Node Cell -> Node Cell
withRange children =
    addToDefaultRange children


constant : String -> Node Cell
constant text =
    createNode ConstantCell
        |> addText "constant" text


inputCell : String -> Node Cell
inputCell text =
    createNode InputCell
        |> addText "input" text


horizStackCell : Node Cell
horizStackCell =
    createNode (StackCell Horiz)


vertStackCell : Node Cell
vertStackCell =
    createNode (StackCell Vert)


placeholderCell : String -> Node Cell
placeholderCell text =
    createNode PlaceholderCell
        |> addText "placeholder" text


addIndent : Node Cell -> Node Cell
addIndent node =
    addBool "indent" True node



-- EDITOR

viewRoot : Node Cell -> Html Msg
viewRoot root =
  div [] (viewCell root)

viewCell : Node Cell -> List (Html Msg)
viewCell cell =
    case getUnderDefault cell of
        Nothing ->
            [ text ("editor empty for " ++ pathOf cell) ]

        Just content ->
            List.foldl viewContent [] content


viewContent : Node Cell -> List (Html Msg) -> List (Html Msg)
viewContent cell html =
    let
        htmlNew =
            case isaOf cell of
                RootCell ->
                    viewStackCell cell Vert

                ConstantCell ->
                    viewConstantCell cell

                InputCell ->
                    viewInputCell cell

                StackCell orientation ->
                    viewStackCell cell orientation

                PlaceholderCell ->
                    viewPlaceholderCell cell
    in
        htmlNew :: List.reverse html |> List.reverse


viewStackCell : Node Cell -> Orientation -> Html Msg
viewStackCell cell orientation =
    case orientation of
        Vert ->
            viewVertStackCell cell

        Horiz ->
            viewHorizStackCell cell


viewVertStackCell : Node Cell -> Html Msg
viewVertStackCell cell =
    let
        indent =
            if boolOf "indent" cell then
                HtmlA.style "margin" "3px 20px"
            else
                HtmlA.style "margin" "0px"
    in
        div [ indent ]
            <| viewCell cell


viewHorizStackCell : Node Cell -> Html Msg
viewHorizStackCell cell =
    div [ HtmlA.style "display" "inline-block" ]
        <| viewCell cell


viewConstantCell : Node Cell -> Html Msg
viewConstantCell cell =
    span [ HtmlA.style "margin" "0px 3px 0px 0px" ] [ text (textOf "constant" cell) ]


viewInputCell : Node Cell -> Html Msg
viewInputCell cell =
    span []
        [ input
            [ HtmlA.style "border-width" "0px"
            , HtmlA.style "border" "none"
            , HtmlA.style "outline" "none"
              --, HtmlA.map (\cellMsg -> PipeMsg cellMsg) (produceKeyboardMsg cell)
            , HtmlA.placeholder "<no value>"
            , HtmlA.value (textOf "input" cell)
              --, HtmlA.map (\cellMsg -> PipeMsg cellMsg) (HtmlE.onInput (Input cell))
            ]
            []
        ]


viewPlaceholderCell : Node Cell -> Html Msg
viewPlaceholderCell cell =
    div []
        [ input
            [ HtmlA.style "border-width" "0px"
            , HtmlA.style "border" "none"
            , HtmlA.style "outline" "none"
              --, HtmlA.map (\cellMsg -> PipeMsg cellMsg) (produceKeyboardMsg cell)
            , HtmlA.placeholder ""
            , HtmlA.value ("<" ++ textOf "placeholder" cell ++ ">")
              --, HtmlA.map (\cellMsg -> PipeMsg cellMsg) (HtmlE.onInput (Input cell))
            , HtmlE.onInput Swallow
            ]
            []
        ]
