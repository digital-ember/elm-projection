module Editor exposing (..)

import Structure exposing (..)
import Html exposing (..)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD


type Cell a
    = ContentCell ContentCell
    | EffectCell (Effect a)


type ContentCell
    = RootCell
    | StackCell Orientation
    | ConstantCell
    | InputCell
    | PlaceholderCell


type Effect a
    = OnEnterEffect
        { source : Node a
        , handler : Node a -> Node a
        }


type Orientation
    = Vert
    | Horiz


type Msg a
    = NoOp
    | Swallow String
    | ReplacePlaceHolder (Effect a)


createRootCell : a -> Node (Cell a)
createRootCell a =
    createRoot (ContentCell RootCell)


with : Node (Cell a) -> Node (Cell a) -> Node (Cell a)
with node =
    addToDefault node


withRange : List (Node (Cell a)) -> Node (Cell a) -> Node (Cell a)
withRange children =
    addToDefaultRange children


constant : String -> a -> Node (Cell a)
constant text a =
    createNode (ContentCell ConstantCell)
        |> addText "constant" text


inputCell : String -> a -> Node (Cell a)
inputCell text a =
    createNode (ContentCell InputCell)
        |> addText "input" text


horizStackCell : a -> Node (Cell a)
horizStackCell a =
    createNode (ContentCell (StackCell Horiz))


vertStackCell : a -> Node (Cell a)
vertStackCell a =
    createNode (ContentCell (StackCell Vert))


placeholderCell : String -> Node (Cell a)
placeholderCell text =
    createNode (ContentCell PlaceholderCell)
        |> addText "placeholder" text


onEnterEffect : Node a -> (Node a -> Node a) -> Node (Cell a)
onEnterEffect source handler =
    createNode
        (EffectCell
            (OnEnterEffect
                { source = source
                , handler = handler
                }
            )
        )


addIndent : Node (Cell a) -> Node (Cell a)
addIndent node =
    addBool "indent" True node



-- BEHAVIOR


editorUpdate : (domainMsg -> Node a -> Node a) -> Msg a -> Node a -> ( Node a, Cmd (Msg a) )
editorUpdate domainUpdate msg domainModel =
    case msg of
        NoOp ->
            ( domainModel, Cmd.none )

        Swallow _ ->
            ( domainModel, Cmd.none )

        ReplacePlaceHolder (OnEnterEffect { source, handler }) ->
            ( handler source, Cmd.none )



-- EDITOR


viewContentOnly : Node (Cell a) -> List (Html (Msg a)) -> List (Html (Msg a))
viewContentOnly root html =
    case isaOf root of
        ContentCell _ ->
            html

        EffectCell _ ->
            []


viewRoot : Node (Cell a) -> Html (Msg a)
viewRoot root =
    div [] <|
        case isaOf root of
            ContentCell _ ->
                viewCell root

            EffectCell _ ->
                []


viewCell : Node (Cell a) -> List (Html (Msg a))
viewCell cell =
    case isaOf cell of
        ContentCell _ ->
            case getUnderDefault cell of
                Nothing ->
                    [ text ("editor empty for " ++ pathOf cell) ]

                Just content ->
                    List.foldl viewContent [] content

        EffectCell _ ->
            []


viewContent : Node (Cell a) -> List (Html (Msg a)) -> List (Html (Msg a))
viewContent cell html =
    case isaOf cell of
        ContentCell ccell ->
            let
                htmlNew =
                    case ccell of
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

        EffectCell _ ->
            []


viewStackCell : Node (Cell a) -> Orientation -> Html (Msg a)
viewStackCell cell orientation =
    case isaOf cell of
        ContentCell _ ->
            case orientation of
                Vert ->
                    viewVertStackCell cell

                Horiz ->
                    viewHorizStackCell cell

        EffectCell _ ->
            text ""


viewVertStackCell : Node (Cell a) -> Html (Msg a)
viewVertStackCell cell =
    case isaOf cell of
        ContentCell _ ->
            let
                indent =
                    if boolOf "indent" cell then
                        HtmlA.style "margin" "3px 20px"
                    else
                        HtmlA.style "margin" "0px"
            in
                div [ indent ] <|
                    viewCell cell

        EffectCell _ ->
            text ""


viewHorizStackCell : Node (Cell a) -> Html (Msg a)
viewHorizStackCell cell =
    case isaOf cell of
        ContentCell _ ->
            div [ HtmlA.style "display" "inline-block" ] <|
                viewCell cell

        EffectCell _ ->
            text ""


viewConstantCell : Node (Cell a) -> Html (Msg a)
viewConstantCell cell =
    case isaOf cell of
        ContentCell _ ->
            span [ HtmlA.style "margin" "0px 3px 0px 0px" ] [ text (textOf "constant" cell) ]

        EffectCell _ ->
            text ""


viewInputCell : Node (Cell a) -> Html (Msg a)
viewInputCell cell =
    case isaOf cell of
        ContentCell _ ->
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

        EffectCell _ ->
            text ""


viewPlaceholderCell : Node (Cell a) -> Html (Msg a)
viewPlaceholderCell cell =
    case isaOf cell of
        ContentCell _ ->
            let
                mbOnEnterEffect =
                    getUnderCustom "onEnter" cell
                        |> Maybe.andThen List.head
                        |> Maybe.andThen
                            (\onEnterCell ->
                                case isaOf onEnterCell of
                                    EffectCell effect ->
                                        Just effect

                                    ContentCell _ ->
                                        Nothing
                            )

                onEnterAttribute =
                    case mbOnEnterEffect of
                        Nothing ->
                            []

                        Just effect ->
                            [ produceKeyboardMsg effect ]
            in
                div []
                    [ input
                        ([ HtmlA.style "border-width" "0px"
                         , HtmlA.style "border" "none"
                         , HtmlA.style "outline" "none"
                         , HtmlA.placeholder ""
                         , HtmlA.value ("<" ++ textOf "placeholder" cell ++ ">")
                           --, HtmlA.map (\cellMsg -> PipeMsg cellMsg) (HtmlE.onInput (Input cell))
                         , HtmlE.onInput Swallow
                         ]
                            ++ onEnterAttribute
                        )
                        []
                    ]

        EffectCell _ ->
            text ""


produceKeyboardMsg : Effect a -> Attribute (Msg a)
produceKeyboardMsg effect =
    let
        canHandle c =
            case c of
                13 ->
                    -- ENTER
                    JsonD.succeed (ReplacePlaceHolder effect)

                _ ->
                    JsonD.fail ("incorrect code: " ++ String.fromInt c)
    in
        HtmlE.on "keydown" (JsonD.andThen canHandle HtmlE.keyCode)
