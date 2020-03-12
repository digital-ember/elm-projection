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
import Dict as Dict
import Html exposing (..)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD
import Browser.Dom as Dom
import Task as Task


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
        { effectInput : ( Node a, Maybe (Node a) )
        , effectHandler : ( Node a, Maybe (Node a) ) -> Node a
        }
    | OnInputEffect
        { effectInput : ( Node a, Path, String )
        , effectHandler : ( Node a, Path, String ) -> String -> Node a
        }
    | NavSelectionEffect
        { dir : Dir
        , cellSelected : Node (Cell a)
        }


type EffectGroup a
    = InputEffectGroup (List (Effect a))
    | KeyboardEffectGroup (List (Effect a))


type Orientation
    = Vert
    | Horiz


type Msg a
    = -- will be needed for selection update
      NoOp
    | Swallow String
    | NavSelection (Effect a)
    | OnEnter (Effect a)
    | OnInput (Effect a) String


type Dir
    = U
    | D
    | L
    | R


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


onEnterEffect : ( Node a, Maybe (Node a) ) -> (( Node a, Maybe (Node a) ) -> Node a) -> Effect a
onEnterEffect effectInput effectHandler =
    OnEnterEffect
        { effectInput = effectInput
        , effectHandler = effectHandler
        }


onInputEffect : ( Node a, Path, String ) -> (( Node a, Path, String ) -> String -> Node a) -> Effect a
onInputEffect effectInput effectHandler =
    OnInputEffect
        { effectInput = effectInput
        , effectHandler = effectHandler
        }


navEffects : Node (Cell a) -> List (Cell a)
navEffects cell =
    [ navEffect U cell
    , navEffect D cell
    , navEffect L cell
    , navEffect R cell
    ]


navEffect : Dir -> Node (Cell a) -> Cell a
navEffect dir cell =
    EffectCell <| NavSelectionEffect { dir = dir, cellSelected = cell }


grouped : List (Cell a) -> List (EffectGroup a)
grouped effectCells =
    let
        updateGroup effect mbEffectList =
            case mbEffectList of
                Nothing ->
                    Just [ effect ]

                Just effectList ->
                    Just <| effect :: effectList

        toDict effectCell groupDict =
            case effectCell of
                ContentCell _ ->
                    groupDict

                EffectCell effect ->
                    case effect of
                        OnInputEffect _ ->
                            Dict.update "input" (updateGroup effect) groupDict

                        OnEnterEffect _ ->
                            Dict.update "keyboard" (updateGroup effect) groupDict

                        NavSelectionEffect _ ->
                            Dict.update "keyboard" (updateGroup effect) groupDict

        dictGrouped =
            List.foldl toDict Dict.empty effectCells

        toEffectGroupList k v effectGroupList =
            case k of
                "input" ->
                    InputEffectGroup v :: effectGroupList

                "keyboard" ->
                    KeyboardEffectGroup v :: effectGroupList

                _ ->
                    effectGroupList
    in
        Dict.foldl toEffectGroupList [] dictGrouped



-- BEHAVIOR


updateEditor : Msg a -> Node (Cell a) -> Node a -> ( Node a, Cmd (Msg a) )
updateEditor msg editorModel domainModel =
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

        NavSelection effect ->
            case effect of
                NavSelectionEffect navData ->
                    ( domainModel, updateSelection editorModel domainModel navData )

                _ ->
                    ( domainModel, Cmd.none )


updateSelection : Node (Cell a) -> Node a -> { dir : Dir, cellSelected : Node (Cell a) } -> Cmd (Msg a)
updateSelection editorModel domainModel { dir, cellSelected } =
    let
        mbOrientation =
            orientationOf editorModel cellSelected

        mover op =
            move editorModel domainModel cellSelected op
    in
        case mbOrientation of
            Nothing ->
                Cmd.none

            Just orientation ->
                case ( dir, orientation ) of
                    ( U, Vert ) ->
                        mover (-)

                    ( D, Vert ) ->
                        mover (+)

                    ( L, Horiz ) ->
                        mover (-)

                    ( R, Horiz ) ->
                        mover (+)

                    _ ->
                        Cmd.none


move : Node (Cell a) -> Node a -> Node (Cell a) -> (Int -> Int -> Int) -> Cmd (Msg a)
move editorModel domainModel cellSelected op =
    let
        nextPathAsId parentPath feature index =
            pathAsId parentPath ++ "-" ++ feature ++ String.fromInt (op index 1)

        pathSelected =
            pathOf cellSelected
    in
        if lengthOf pathSelected == 1 then
            --root or error
            Cmd.none
        else
            let
                split =
                    splitLastPathSegment pathSelected
            in
                case split of
                    ( Nothing, _ ) ->
                        Cmd.none

                    ( _, Nothing ) ->
                        Cmd.none

                    ( Just { feature, index }, Just parentPath ) ->
                        Task.attempt (\_ -> NoOp) (Dom.focus <| nextPathAsId parentPath feature index)



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
                    [ text "" ]

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
                div
                    [ indent
                    , HtmlA.id (pathAsIdFromNode cell)
                    ]
                <|
                    viewCell cell

        EffectCell _ ->
            text ""


viewHorizStackCell : Node (Cell a) -> Html (Msg a)
viewHorizStackCell cell =
    case isaOf cell of
        ContentCell _ ->
            div
                [ HtmlA.style "display" "flex"
                , HtmlA.id (pathAsIdFromNode cell)
                ]
            <|
                viewCell cell

        EffectCell _ ->
            text ""


viewConstantCell : Node (Cell a) -> Html (Msg a)
viewConstantCell cell =
    case isaOf cell of
        ContentCell _ ->
            div []
                [ label
                    [ HtmlA.style "margin" "0px 3px 0px 0px"
                    , HtmlA.id (pathAsIdFromNode cell)
                    ]
                    [ text (textOf "constant" cell) ]
                ]

        EffectCell _ ->
            text ""


viewInputCell : Node (Cell a) -> Html (Msg a)
viewInputCell cell =
    case isaOf cell of
        ContentCell _ ->
            div
                [ HtmlA.style "margin" "0px 3px 0px 0px"
                ]
                [ input
                    ([ HtmlA.style "border-width" "0px"
                     , HtmlA.style "border" "none"
                     , HtmlA.style "outline" "none"
                     , HtmlA.placeholder "<no value>"
                     , HtmlA.value (textOf "input" cell)
                     , HtmlA.id (pathAsIdFromNode cell)
                     ]
                        ++ createInputCellAttributes cell
                    )
                    []
                ]

        EffectCell _ ->
            text ""


viewPlaceholderCell : Node (Cell a) -> Html (Msg a)
viewPlaceholderCell cell =
    case isaOf cell of
        ContentCell _ ->
            div []
                [ input
                    ([ HtmlA.style "border-width" "0px"
                     , HtmlA.style "border" "none"
                     , HtmlA.style "outline" "none"
                     , HtmlA.style "color" "#888888"
                     , HtmlA.style "font-style" "italic"
                     , HtmlA.value ("<" ++ textOf "placeholder" cell ++ ">")
                     , HtmlE.onInput Swallow
                     , HtmlA.id (pathAsIdFromNode cell)
                     ]
                        ++ createInputCellAttributes cell
                    )
                    []
                ]

        EffectCell _ ->
            text ""


createInputCellAttributes : Node (Cell a) -> List (Attribute (Msg a))
createInputCellAttributes cell =
    let
        effectGroups =
            grouped <|
                isasUnderCustom "effects" cell
                    ++ navEffects cell
    in
        List.map attributeFromEffectGroup effectGroups
            |> List.filterMap identity


attributeFromEffectGroup : EffectGroup a -> Maybe (Attribute (Msg a))
attributeFromEffectGroup effectGroup =
    case effectGroup of
        InputEffectGroup effects ->
            case effects of
                effect :: [] ->
                    Just (effectAttributeFromInput (OnInput effect))

                _ ->
                    Nothing

        KeyboardEffectGroup effects ->
            Just (effectAttributeFromKey (inputEffectMap effects))


keyFromDir : Dir -> String
keyFromDir dir =
    case dir of
        U ->
            "ArrowUp"

        D ->
            "ArrowDown"

        L ->
            "ArrowLeft"

        R ->
            "ArrowRight"


effectAttributeFromInput : (String -> Msg a) -> Attribute (Msg a)
effectAttributeFromInput handler =
    HtmlE.onInput handler


inputEffectMap : List (Effect a) -> Dict.Dict String (Msg a)
inputEffectMap effects =
    List.foldl
        (\effect dict ->
            case effect of
                OnEnterEffect _ ->
                    Dict.insert "Enter" (OnEnter effect) dict

                NavSelectionEffect { dir } ->
                    Dict.insert (keyFromDir dir) (NavSelection effect) dict

                OnInputEffect _ ->
                    dict
        )
        Dict.empty
        effects


effectAttributeFromKey : Dict.Dict String (Msg a) -> Attribute (Msg a)
effectAttributeFromKey dictKeyToMsg =
    let
        canHandle k =
            let
                mbMsg =
                    Dict.get k dictKeyToMsg
            in
                case mbMsg of
                    Nothing ->
                        JsonD.fail ("incorrect code: " ++ k)

                    Just msg ->
                        JsonD.succeed msg
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


{-| Defaults to Vert in case of EffectCell, root or unexpected behavior
-}
orientationOf : Node (Cell a) -> Node (Cell a) -> Maybe Orientation
orientationOf root cell =
    case isaOf cell of
        ContentCell (StackCell o) ->
            Just o

        ContentCell _ ->
            parentOf root (pathOf cell)
                |> Maybe.andThen (orientationOf root)

        _ ->
            Nothing
