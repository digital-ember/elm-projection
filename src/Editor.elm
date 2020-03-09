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
                    renderStackCell cell Vert

                ConstantCell ->
                    renderConstantCell cell

                InputCell ->
                    renderInputCell cell

                StackCell orientation ->
                    renderStackCell cell orientation

                PlaceholderCell ->
                    renderPlaceholderCell cell
    in
        htmlNew :: List.reverse html |> List.reverse


renderStackCell : Node Cell -> Orientation -> Html Msg
renderStackCell cell orientation =
    case orientation of
        Vert ->
            renderVertStackCell cell

        Horiz ->
            renderHorizStackCell cell


renderVertStackCell : Node Cell -> Html Msg
renderVertStackCell cell =
    let
        indent =
            if propertyBoolValueOf cell "indent" |> Maybe.withDefault False then
                HtmlA.style "margin" "3px 20px"
            else
                HtmlA.style "margin" "0px"
    in
        div [ indent ]
            <| viewCell cell


renderHorizStackCell : Node Cell -> Html Msg
renderHorizStackCell cell =
    div [ HtmlA.style "display" "inline-block" ]
        <| viewCell cell


renderConstantCell : Node Cell -> Html Msg
renderConstantCell cell =
    span [ HtmlA.style "margin" "0px 3px 0px 0px" ] [ text (propertyStringValueOf cell "constant" |> Maybe.withDefault "") ]


renderInputCell : Node Cell -> Html Msg
renderInputCell cell =
    span []
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


renderPlaceholderCell : Node Cell -> Html Msg
renderPlaceholderCell cell =
    div []
        [ input
            [ HtmlA.style "border-width" "0px"
            , HtmlA.style "border" "none"
            , HtmlA.style "outline" "none"
              --, HtmlA.map (\cellMsg -> PipeMsg cellMsg) (produceKeyboardMsg cell)
            , HtmlA.placeholder ""
            , HtmlA.value ("<" ++ (propertyStringValueOf cell "placeholder" |> Maybe.withDefault "no values") ++ ">")
              --, HtmlA.map (\cellMsg -> PipeMsg cellMsg) (HtmlE.onInput (Input cell))
            , HtmlE.onInput Swallow
            ]
            []
        ]
