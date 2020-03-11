module Editor
    exposing
        ( Cell
        , Msg
        , createRootCell
        , with
        , withRange
        , constantCell
        , inputCell
        , horizStackCell
        , vertStackCell
        , placeholderCell
        , addIndent
        , withEffect
        , onEnterEffect
        , onInputEffect
        , updateEditor
        , viewEditor
        )

import Structure exposing (..)
import Html exposing (..)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD


--import Browser.Dom as Dom


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
        { effectInput : Node a
        , effectHandler : Node a -> Node a
        }
    | OnInputEffect
        { effectInput : ( Node a, Path )
        , effectHandler : ( Node a, Path ) -> String -> Node a
        }


type Orientation
    = Vert
    | Horiz


type Msg a
    = -- will be needed for selection update
      NoOp
    | Swallow String
    | OnEnter (Effect a)
    | OnInput (Effect a) String


createRootCell : Node (Cell a)
createRootCell =
    createRoot (ContentCell RootCell)


with : Node (Cell a) -> Node (Cell a) -> Node (Cell a)
with node =
    addToDefault node


withRange : List (Node (Cell a)) -> Node (Cell a) -> Node (Cell a)
withRange children =
    addToDefaultRange children


constantCell : String -> Node (Cell a)
constantCell text =
    createNode (ContentCell ConstantCell)
        |> addText "constant" text


inputCell : String -> Node (Cell a)
inputCell text =
    createNode (ContentCell InputCell)
        |> addText "input" text


horizStackCell : Node (Cell a)
horizStackCell =
    createNode (ContentCell (StackCell Horiz))


vertStackCell : Node (Cell a)
vertStackCell =
    createNode (ContentCell (StackCell Vert))


placeholderCell : String -> Node (Cell a)
placeholderCell text =
    createNode (ContentCell PlaceholderCell)
        |> addText "placeholder" text


addIndent : Node (Cell a) -> Node (Cell a)
addIndent node =
    addBool "indent" True node



-- EFFECTS


withEffect : Effect a -> Node (Cell a) -> Node (Cell a)
withEffect effect =
    addToCustom "effects" <|
        createNode <|
            EffectCell effect


onEnterEffect : Node a -> (Node a -> Node a) -> Effect a
onEnterEffect effectInput effectHandler =
    OnEnterEffect
        { effectInput = effectInput
        , effectHandler = effectHandler
        }


onInputEffect : ( Node a, Path ) -> (( Node a, Path ) -> String -> Node a) -> Effect a
onInputEffect effectInput effectHandler =
    OnInputEffect
        { effectInput = effectInput
        , effectHandler = effectHandler
        }



-- BEHAVIOR


updateEditor : Msg a -> Node a -> ( Node a, Cmd (Msg a) )
updateEditor msg domainModel =
    case msg of
        NoOp ->
            ( domainModel, Cmd.none )

        Swallow _ ->
            ( domainModel, Cmd.none )

        OnEnter effect ->
            case effect of
                OnEnterEffect { effectInput, effectHandler } ->
                    ( effectHandler effectInput |> updatePaths, Cmd.none )

                _ ->
                    ( domainModel, Cmd.none )

        OnInput effect value ->
            case effect of
                OnInputEffect { effectInput, effectHandler } ->
                    ( effectHandler effectInput value |> updatePaths, Cmd.none )

                _ ->
                    ( domainModel, Cmd.none )



-- EDITOR


viewEditor : Node (Cell a) -> Html (Msg a)
viewEditor root =
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
                    [ text "editor empty" ]

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
            div [ HtmlA.style "display" "flex" ] <|
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
            let
                effects =
                    isasUnderCustom "effects" cell

                effectAttributes =
                    List.map toEffectAttribute effects
                        |> List.filterMap identity
            in
                div []
                    [ input
                        ([ HtmlA.style "border-width" "0px"
                         , HtmlA.style "border" "none"
                         , HtmlA.style "outline" "none"
                           --, HtmlA.map (\cellMsg -> PipeMsg cellMsg) (produceKeyboardMsg cell)
                         , HtmlA.placeholder "<no value>"
                         , HtmlA.value (textOf "input" cell)
                           --, HtmlA.map (\cellMsg -> PipeMsg cellMsg) (HtmlE.onInput (Input cell))
                         ]
                            ++ effectAttributes
                        )
                        []
                    ]

        EffectCell _ ->
            text ""


viewPlaceholderCell : Node (Cell a) -> Html (Msg a)
viewPlaceholderCell cell =
    case isaOf cell of
        ContentCell _ ->
            let
                effects =
                    isasUnderCustom "effects" cell

                effectAttributes =
                    List.map toEffectAttribute effects
                        |> List.filterMap identity
            in
                div []
                    [ input
                        ([ HtmlA.style "border-width" "0px"
                         , HtmlA.style "border" "none"
                         , HtmlA.style "outline" "none"
                         , HtmlA.style "color" "#888888"
                         , HtmlA.style "font-style" "italic"
                         , HtmlA.value ("<" ++ textOf "placeholder" cell ++ ">")
                         , HtmlE.onInput Swallow
                         ]
                            ++ effectAttributes
                        )
                        []
                    ]

        EffectCell _ ->
            text ""


toEffectAttribute : Cell a -> Maybe (Attribute (Msg a))
toEffectAttribute cell =
    case cell of
        EffectCell effect ->
            Just (produceEffectAttribute effect)

        ContentCell _ ->
            Nothing


produceEffectAttribute : Effect a -> Attribute (Msg a)
produceEffectAttribute effect =
    case effect of
        OnInputEffect _ ->
            effectAttributeFromInput (OnInput effect)

        OnEnterEffect _ ->
            effectAttributeFromKey "Enter" (OnEnter effect)


effectAttributeFromInput : (String -> Msg a) -> Attribute (Msg a)
effectAttributeFromInput handler =
    HtmlE.onInput handler


effectAttributeFromKey : String -> Msg a -> Attribute (Msg a)
effectAttributeFromKey key msg =
    let
        canHandle k =
            if k == key then
                JsonD.succeed msg
            else
                JsonD.fail ("incorrect code: " ++ k)
    in
        HtmlE.on "keydown" <|
            JsonD.andThen canHandle <|
                JsonD.field "key" JsonD.string


isasUnderCustom : String -> Node a -> List a
isasUnderCustom featureKey parent =
    let
        mbChildren =
            if featureKey == "" then
                getUnderDefault parent
            else
                getUnderCustom featureKey parent
    in
        mbChildren
            |> Maybe.andThen
                (\children ->
                    Just (List.map isaOf children)
                )
            |> Maybe.withDefault []
