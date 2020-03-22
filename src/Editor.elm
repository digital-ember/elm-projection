module Editor exposing
    ( Cell
    , EditorModel
    , MarginSide(..)
    , Msg(..)
    , addIndent
    , addMargin
    , buttonCell
    , constantCell
    , deletionEffect
    , edgeCell
    , graphCell
    , griddify
    , horizSplitCell
    , horizStackCell
    , initEditorModel
    , inputCell
    , inputEffect
    , insertionEffect
    , placeholderCell
    , refCell
    , replacementEffect
    , rootCell
    , updateEditor
    , vertGridCell
    , vertSplitCell
    , vertStackCell
    , vertexCell
    , viewEditor
    , with
    , withEffect
    , withRange
    )

import Browser.Dom as Dom
import Color
import Dict as Dict
import Force exposing (Entity, Force, State)
import Html exposing (..)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD
import Structure exposing (..)
import Task as Task
import TypedSvg exposing (g, line, rect, svg, text_)
import TypedSvg.Attributes exposing (fill, stroke, textAnchor, viewBox)
import TypedSvg.Attributes.InPx exposing (height, rx, ry, strokeWidth, width, x, x1, x2, y, y1, y2)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..))


type Cell a
    = ContentCell ContentCell
    | EffectCell (EffectCell a)


type ContentCell
    = SplitCell
    | RootCell
    | StackCell
    | ConstantCell
    | InputCell
    | PlaceholderCell
    | ButtonCell
    | RefCell
    | GraphCell
    | VertexCell
    | EdgeCell


type EffectCell a
    = InsertionEffect (InsertionEffectData a)
    | DeletionEffect DeletionEffectData
    | InputEffect InputEffectData
    | NavSelectionEffect NavSelectionEffectData
    | CreateScopeEffect (CreateScopeEffectData a)


type alias EditorModel a =
    { dRoot : Node a
    , eRoot : Node (Cell a)
    , mbSimulation : Maybe (Force.State String)
    }


type alias InsertionEffectData a =
    { path : Path
    , nodeToInsert : Node a
    , isReplace : Bool
    , feature : String
    }


type alias DeletionEffectData =
    { path : Path
    , selection : Selection
    }


type alias InputEffectData =
    { path : Path
    , key : String
    }


type alias NavSelectionEffectData =
    { dir : Dir
    , pathSelectedCell : Path
    , selection : Selection
    }


type alias CreateScopeEffectData a =
    { isa : a
    , pathContextNode : Path
    , scopeProvider : Maybe (List String)
    }


type alias Selection =
    { start : Int
    , end : Int
    , dir : String
    }


type EffectGroup a
    = InputEffectGroup (List (EffectCell a))
    | KeyboardEffectGroup (List (EffectCell a))
    | FocusEffectGroup (List (EffectCell a))


type Orientation
    = Vert
    | Horiz


type Msg a
    = NoOp
    | Swallow String
    | NavSelection (EffectCell a)
    | OnEnter (EffectCell a) (Node (Cell a))
    | OnClick (EffectCell a) (Node (Cell a))
    | OnBackspace (EffectCell a) (Node (Cell a))
    | OnDelete (EffectCell a) (Node (Cell a))
    | OnInput (EffectCell a) String
    | UpdateScope (EffectCell a)
    | Tick


type Dir
    = U
    | D
    | L
    | R


type MarginSide
    = Top
    | Right
    | Bottom
    | Left


initEditorModel : Node a -> Node (Cell a) -> EditorModel a
initEditorModel dRoot eRoot =
    { dRoot = dRoot
    , eRoot = eRoot
    , mbSimulation = Nothing
    }


rootCell : Node (Cell a)
rootCell =
    createRoot (ContentCell RootCell)


constantCell : String -> Node (Cell a)
constantCell text =
    createNode (ContentCell ConstantCell)
        |> addText propConstant text


refCell : a -> String -> Node a -> Maybe (List String) -> Node (Cell a)
refCell target text nodeContext scopeProvider =
    let
        pathContext =
            pathOf nodeContext
    in
    createNode (ContentCell RefCell)
        |> addText propInput (textOf text nodeContext)
        |> addRangeToCustom featScope (createRefScope nodeContext)
        |> withEffect (inputEffect pathContext text)
        |> withEffect (createScopeEffect target (pathOf nodeContext) scopeProvider)


createRefScope nodeContext =
    List.map (\scopeElement -> constantCell (textOf propScopeValue scopeElement)) (getUnderCustom featScope nodeContext)


inputCell : String -> Node a -> Node (Cell a)
inputCell text nodeContext =
    createNode (ContentCell InputCell)
        |> addText propInput (textOf text nodeContext)
        |> withEffect (inputEffect (pathOf nodeContext) text)


horizStackCell : Node (Cell a)
horizStackCell =
    createNode (ContentCell StackCell)
        |> addBool propIsHoriz True


vertStackCell : Node (Cell a)
vertStackCell =
    createNode (ContentCell StackCell)
        |> addBool propIsHoriz False


vertSplitCell : Node (Cell a)
vertSplitCell =
    createNode (ContentCell SplitCell)


horizSplitCell : Node (Cell a)
horizSplitCell =
    createNode (ContentCell SplitCell)
        |> addBool propIsHoriz True


vertGridCell : Node (Cell a)
vertGridCell =
    vertStackCell
        |> addBool propIsGrid True


placeholderCell : String -> Node (Cell a)
placeholderCell text =
    createNode (ContentCell PlaceholderCell)
        |> addText propPlaceholder text


buttonCell : String -> Node (Cell a)
buttonCell text =
    createNode (ContentCell ButtonCell)
        |> addText propText text


graphCell : Node (Cell a)
graphCell =
    createNode (ContentCell GraphCell)


vertexCell : String -> Node a -> Node (Cell a)
vertexCell text nodeContext =
    createNode (ContentCell VertexCell)
        |> addText propText (textOf text nodeContext)


edgeCell : String -> ( String, String ) -> Node a -> Node (Cell a)
edgeCell text ( from, to ) nodeContext =
    createNode (ContentCell EdgeCell)
        |> addText propText (textOf text nodeContext)
        |> addText propFrom from
        |> addText propTo to


with : Node (Cell a) -> Node (Cell a) -> Node (Cell a)
with node =
    addToDefault node


withRange : List (Node (Cell a)) -> Node (Cell a) -> Node (Cell a)
withRange children =
    addRangeToDefault children


addIndent : Node (Cell a) -> Node (Cell a)
addIndent node =
    addBool propIndent True node


addMargin : MarginSide -> Int -> Node (Cell a) -> Node (Cell a)
addMargin side space node =
    let
        key =
            case side of
                Top ->
                    propMarginTop

                Right ->
                    propMarginRight

                Bottom ->
                    propMarginBottom

                Left ->
                    propMarginLeft
    in
    addInt key space node



-- EFFECTS


withEffect : EffectCell a -> Node (Cell a) -> Node (Cell a)
withEffect effect =
    addToCustom featEffects <|
        createNode <|
            EffectCell effect


replacementEffect : String -> Node a -> Node a -> EffectCell a
replacementEffect feature nodeContext nodeToInsert =
    InsertionEffect <|
        InsertionEffectData (pathOf nodeContext) nodeToInsert True feature


insertionEffect : Node a -> Node a -> EffectCell a
insertionEffect nodeContext nodeToInsert =
    InsertionEffect <|
        InsertionEffectData (pathOf nodeContext) nodeToInsert False ""


deletionEffect : Node a -> EffectCell a
deletionEffect nodeContext =
    DeletionEffect <|
        DeletionEffectData (pathOf nodeContext) emptySelection


inputEffect : Path -> String -> EffectCell a
inputEffect path key =
    InputEffect <| InputEffectData path key


createScopeEffect : a -> Path -> Maybe (List String) -> EffectCell a
createScopeEffect target path scopeProvider =
    CreateScopeEffect <|
        CreateScopeEffectData target path scopeProvider


navEffects : Path -> List (Cell a)
navEffects path =
    [ navEffect U path
    , navEffect D path
    , navEffect L path
    , navEffect R path
    ]


navEffect : Dir -> Path -> Cell a
navEffect dir path =
    EffectCell <|
        NavSelectionEffect
            { dir = dir
            , pathSelectedCell = path
            , selection = emptySelection
            }


emptySelection : Selection
emptySelection =
    { start = -1
    , end = -1
    , dir = ""
    }


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
                        InputEffect _ ->
                            Dict.update "input" (updateGroup effect) groupDict

                        InsertionEffect _ ->
                            Dict.update "keyboard" (updateGroup effect) groupDict

                        DeletionEffect _ ->
                            Dict.update "keyboard" (updateGroup effect) groupDict

                        NavSelectionEffect _ ->
                            Dict.update "keyboard" (updateGroup effect) groupDict

                        CreateScopeEffect _ ->
                            Dict.update "focus" (updateGroup effect) groupDict

        dictGrouped =
            List.foldl toDict Dict.empty effectCells

        toEffectGroupList k v effectGroupList =
            case k of
                "input" ->
                    InputEffectGroup v :: effectGroupList

                "keyboard" ->
                    KeyboardEffectGroup v :: effectGroupList

                "focus" ->
                    FocusEffectGroup v :: effectGroupList

                _ ->
                    effectGroupList
    in
    Dict.foldl toEffectGroupList [] dictGrouped



-- BEHAVIOR


updateEditor : Msg a -> EditorModel a -> ( Bool, EditorModel a, Cmd (Msg a) )
updateEditor msg editorModel =
    case msg of
        Tick ->
            tickGraphSimulations editorModel

        NoOp ->
            noUpdate editorModel

        Swallow _ ->
            noUpdate editorModel

        OnEnter effect cellContext ->
            updateOnInsertionEffect editorModel effect cellContext

        OnClick effect cellContext ->
            updateOnInsertionEffect editorModel effect cellContext

        OnDelete effect cellContext ->
            updateOnDeleteEffect editorModel effect cellContext

        OnBackspace effect cellContext ->
            updateOnBackspaceEffect editorModel effect cellContext

        OnInput effect value ->
            updateOnInputEffect editorModel effect value

        NavSelection effect ->
            ( False, editorModel, updateOnNavEffect effect editorModel.eRoot )

        UpdateScope effect ->
            updateOnCreateScopeEffect editorModel effect


tickGraphSimulations : EditorModel a -> ( Bool, EditorModel a, Cmd (Msg a) )
tickGraphSimulations editorModel =
    let
        mbCellGraph =
            nodesOf (ContentCell GraphCell) editorModel.eRoot |> List.head
    in
    case mbCellGraph of
        Nothing ->
            noUpdate editorModel

        Just cellGraph ->
            case editorModel.mbSimulation of
                Nothing ->
                    let
                        verticies =
                            nodesOf (ContentCell VertexCell) cellGraph

                        forces =
                            [ Force.customLinks 1 <| customEdgeForcesFromGraph cellGraph
                            , Force.manyBodyStrength -1000 <| List.map (\v -> pathAsIdFromNode v) <| verticies
                            , Force.center 400 300
                            ]
                                |> Debug.log "forces"
                    in
                    ( False, { editorModel | mbSimulation = Just <| Force.simulation forces }, Cmd.none )

                Just simulation ->
                    let
                        pathToGraph =
                            pathOf cellGraph

                        verticies =
                            nodesOf (ContentCell VertexCell) cellGraph

                        edges =
                            nodesOf (ContentCell EdgeCell) cellGraph

                        addPosToCell e =
                            e.value
                                |> addFloat propX e.x
                                |> addFloat propY e.y

                        ( newSimulationState, verticiesNew ) =
                            List.indexedMap (\i v -> forceEntityFromVertex i v) verticies
                                |> Force.tick simulation

                        childrenNew =
                            List.map addPosToCell verticiesNew
                                ++ edges

                        cellGraphNew =
                            replaceUnderFeature "default" childrenNew cellGraph

                        eRootNew =
                            replaceChildAtPath cellGraphNew pathToGraph editorModel.eRoot
                    in
                    ( False, { editorModel | eRoot = eRootNew, mbSimulation = Just <| newSimulationState }, Cmd.none )


updateOnInsertionEffect : EditorModel a -> EffectCell a -> Node (Cell a) -> ( Bool, EditorModel a, Cmd (Msg a) )
updateOnInsertionEffect editorModel effect cellContext =
    case effect of
        InsertionEffect { path, nodeToInsert, isReplace, feature } ->
            if isReplace then
                let
                    dRootNew =
                        addChildAtPath feature nodeToInsert path editorModel.dRoot |> updatePaths
                in
                ( True, { editorModel | dRoot = dRootNew }, Cmd.none )

            else
                let
                    dRootNew =
                        insertChildAfterPath nodeToInsert path editorModel.dRoot |> updatePaths
                in
                ( True, { editorModel | dRoot = dRootNew }, updateSelectionOnEnter cellContext )

        _ ->
            noUpdate editorModel


updateOnDeleteEffect : EditorModel a -> EffectCell a -> Node (Cell a) -> ( Bool, EditorModel a, Cmd (Msg a) )
updateOnDeleteEffect editorModel effect cellContext =
    case effect of
        DeletionEffect ({ selection } as effectData) ->
            let
                textLength =
                    textOf propInput cellContext |> String.length

                isAtDeletePos =
                    selection.end == textLength
            in
            tryDelete
                editorModel
                effectData
                nextSibling
                textLength
                isAtDeletePos

        _ ->
            noUpdate editorModel


updateOnBackspaceEffect : EditorModel a -> EffectCell a -> Node (Cell a) -> ( Bool, EditorModel a, Cmd (Msg a) )
updateOnBackspaceEffect editorModel effect cellContext =
    case effect of
        DeletionEffect ({ selection } as effectData) ->
            let
                textLength =
                    textOf propInput cellContext |> String.length

                isAtDeletePos =
                    selection.start == 0
            in
            tryDelete
                editorModel
                effectData
                previousSibling
                textLength
                isAtDeletePos

        _ ->
            noUpdate editorModel


updateOnInputEffect : EditorModel a -> EffectCell a -> String -> ( Bool, EditorModel a, Cmd (Msg a) )
updateOnInputEffect editorModel effect value =
    case effect of
        InputEffect { path, key } ->
            ( True, { editorModel | dRoot = updatePropertyByPath editorModel.dRoot path ( key, value ) |> updatePaths }, Cmd.none )

        _ ->
            noUpdate editorModel


updateOnNavEffect : EffectCell a -> Node (Cell a) -> Cmd (Msg a)
updateOnNavEffect effect editorModel =
    case effect of
        NavSelectionEffect navData ->
            updateSelection editorModel navData

        _ ->
            Cmd.none


updateOnCreateScopeEffect : EditorModel a -> EffectCell a -> ( Bool, EditorModel a, Cmd (Msg a) )
updateOnCreateScopeEffect editorModel effect =
    case effect of
        CreateScopeEffect scopeData ->
            ( True, { editorModel | dRoot = setScopeInformation editorModel.dRoot scopeData }, Cmd.none )

        _ ->
            noUpdate editorModel


noUpdate editorModel =
    ( False, editorModel, Cmd.none )


setScopeInformation : Node a -> CreateScopeEffectData a -> Node a
setScopeInformation domainModel scopeData =
    let
        optionNodes =
            nodesOf scopeData.isa domainModel
                |> List.map (\s -> createNode scopeData.isa |> addText propScopeValue (textOf propName s))
    in
    replaceRangeAtPath
        featScope
        optionNodes
        scopeData.pathContextNode
        domainModel


tryDelete : EditorModel a -> DeletionEffectData -> (Node a -> Path -> Maybe (Node a)) -> Int -> Bool -> ( Bool, EditorModel a, Cmd (Msg a) )
tryDelete editorModel { path } navFun textLength isAtDeletePos =
    if textLength == 0 then
        ( True, { editorModel | dRoot = deleteNodeUnder path editorModel.dRoot |> updatePaths }, Cmd.none )

    else if isAtDeletePos then
        let
            mbNext =
                navFun editorModel.dRoot path
        in
        case mbNext of
            Nothing ->
                noUpdate editorModel

            Just next ->
                ( True, { editorModel | dRoot = deleteNodeUnder (pathOf next) editorModel.dRoot |> updatePaths }, Cmd.none )

    else
        noUpdate editorModel


updateSelectionOnEnter : Node (Cell a) -> Cmd (Msg a)
updateSelectionOnEnter cellContext =
    Task.perform
        NavSelection
    <|
        Task.succeed <|
            NavSelectionEffect <|
                NavSelectionEffectData D (pathOf cellContext) <|
                    Selection 0 0 ""


updateSelection : Node (Cell a) -> NavSelectionEffectData -> Cmd (Msg a)
updateSelection editorModel navData =
    let
        mbNodeContext =
            nodeAt editorModel navData.pathSelectedCell

        mbOrientation =
            mbNodeContext |> Maybe.andThen (orientationOf editorModel)
    in
    case mbOrientation of
        Nothing ->
            Cmd.none

        Just orientation ->
            updateSelectionByOrientation editorModel navData orientation


updateSelectionByOrientation : Node (Cell a) -> NavSelectionEffectData -> Orientation -> Cmd (Msg a)
updateSelectionByOrientation editorModel navData orientation =
    let
        mbCellSelected =
            nodeAt editorModel navData.pathSelectedCell
    in
    case mbCellSelected of
        Nothing ->
            Cmd.none

        Just cellSelected ->
            let
                moverTask f =
                    Task.attempt
                        (\_ -> NoOp)
                        (Dom.focus <| pathAsIdFromNode (f editorModel cellSelected))
            in
            case ( navData.dir, orientation ) of
                ( U, Vert ) ->
                    moverTask findPrevInputCell

                ( D, Vert ) ->
                    moverTask findNextInputCell

                ( L, Horiz ) ->
                    if navData.selection.start == 0 then
                        moverTask findPrevInputCell

                    else
                        Cmd.none

                ( R, Horiz ) ->
                    if navData.selection.start >= (textOf propInput cellSelected |> String.length) then
                        moverTask findNextInputCell

                    else
                        Cmd.none

                _ ->
                    Cmd.none


findPrevInputCell : Node (Cell a) -> Node (Cell a) -> Node (Cell a)
findPrevInputCell root current =
    let
        mbPrev =
            previousSibling root (pathOf current)
    in
    case mbPrev of
        Just prev ->
            findPrevInputCellRec root prev
                |> Maybe.withDefault current

        Nothing ->
            let
                mbParent =
                    parentOf root (pathOf current)
            in
            case mbParent of
                Nothing ->
                    current

                Just parent ->
                    findPrevInputCell root parent


findPrevInputCellRec : Node (Cell a) -> Node (Cell a) -> Maybe (Node (Cell a))
findPrevInputCellRec root prev =
    case isaOf prev of
        ContentCell InputCell ->
            Just prev

        ContentCell StackCell ->
            -- down
            case getUnderDefault prev of
                [] ->
                    Just <| findPrevInputCell root prev

                children ->
                    let
                        mbLast =
                            findFirstInputCellRec root (List.reverse children) findPrevInputCellRec
                    in
                    case mbLast of
                        Nothing ->
                            Just <| findPrevInputCell root prev

                        Just last ->
                            Just last

        _ ->
            Just <| findPrevInputCell root prev


findNextInputCell : Node (Cell a) -> Node (Cell a) -> Node (Cell a)
findNextInputCell root current =
    let
        mbNext =
            nextSibling root (pathOf current)
    in
    case mbNext of
        Just next ->
            findNextInputCellRec root next
                |> Maybe.withDefault current

        Nothing ->
            let
                mbParent =
                    parentOf root (pathOf current)
            in
            case mbParent of
                Nothing ->
                    current

                Just parent ->
                    findNextInputCell root parent


findNextInputCellRec : Node (Cell a) -> Node (Cell a) -> Maybe (Node (Cell a))
findNextInputCellRec root next =
    case isaOf next of
        ContentCell InputCell ->
            Just next

        ContentCell StackCell ->
            -- down
            case getUnderDefault next of
                [] ->
                    Just <| findNextInputCell root next

                children ->
                    let
                        mbFirst =
                            findFirstInputCellRec root children findNextInputCellRec
                    in
                    case mbFirst of
                        Nothing ->
                            Just <| findNextInputCell root next

                        Just first ->
                            Just first

        _ ->
            Just <| findNextInputCell root next


findFirstInputCellRec : Node (Cell a) -> List (Node (Cell a)) -> (Node (Cell a) -> Node (Cell a) -> Maybe (Node (Cell a))) -> Maybe (Node (Cell a))
findFirstInputCellRec root candidates recFun =
    case candidates of
        [] ->
            Nothing

        head :: tail ->
            let
                mbFirst =
                    recFun root head
            in
            case mbFirst of
                Nothing ->
                    findFirstInputCellRec root tail recFun

                Just first ->
                    Just first



-- EDITOR


viewEditor : Node (Cell a) -> Html (Msg a)
viewEditor root =
    div [ HtmlA.style "font-family" "Consolas" ] <|
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
                [] ->
                    [ text "" ]

                children ->
                    List.foldl viewContent [] children

        EffectCell _ ->
            []


viewContent : Node (Cell a) -> List (Html (Msg a)) -> List (Html (Msg a))
viewContent cell html =
    case isaOf cell of
        ContentCell ccell ->
            let
                htmlNew =
                    case ccell of
                        SplitCell ->
                            viewSplitCell cell

                        RootCell ->
                            viewStackCell cell

                        ConstantCell ->
                            viewConstantCell cell

                        InputCell ->
                            viewInputCell cell

                        StackCell ->
                            viewStackCell cell

                        PlaceholderCell ->
                            viewPlaceholderCell cell

                        ButtonCell ->
                            viewButtonCell cell

                        RefCell ->
                            viewRefCell cell

                        GraphCell ->
                            viewGraphCell cell

                        VertexCell ->
                            text ""

                        EdgeCell ->
                            text ""
            in
            htmlNew :: List.reverse html |> List.reverse

        EffectCell _ ->
            []


viewSplitCell : Node (Cell a) -> Html (Msg a)
viewSplitCell cell =
    case isaOf cell of
        ContentCell _ ->
            let
                bO =
                    boolOf propIsHoriz cell
            in
            if bO then
                viewHorizSplit cell

            else
                viewVertSplit cell

        EffectCell _ ->
            text ""


viewVertSplit : Node (Cell a) -> Html (Msg a)
viewVertSplit cell =
    case isaOf cell of
        ContentCell _ ->
            let
                ( left, right ) =
                    case getUnderDefault cell of
                        [] ->
                            ( [ text "Completely empty split cell" ], [ text "" ] )

                        first :: [] ->
                            ( viewContent first []
                            , [ text "Empty right side" ]
                            )

                        first :: (second :: _) ->
                            ( viewContent first []
                            , viewContent second []
                            )
            in
            div []
                [ div
                    [ HtmlA.class "split left" ]
                    left
                , div
                    [ HtmlA.class "split right" ]
                    right
                ]

        EffectCell _ ->
            text ""


viewHorizSplit : Node (Cell a) -> Html (Msg a)
viewHorizSplit cell =
    case isaOf cell of
        ContentCell _ ->
            let
                ( top, bottom ) =
                    case getUnderDefault cell of
                        [] ->
                            ( [ text "Completely empty split cell" ], [ text "" ] )

                        first :: [] ->
                            ( viewCell first
                            , [ text "Empty bottom side" ]
                            )

                        first :: (second :: _) ->
                            ( viewCell first
                            , viewCell second
                            )
            in
            div []
                [ div
                    [ HtmlA.class "split top" ]
                    top
                , div
                    [ HtmlA.class "split bottom" ]
                    bottom
                ]

        EffectCell _ ->
            text ""


viewStackCell : Node (Cell a) -> Html (Msg a)
viewStackCell cell =
    case isaOf cell of
        ContentCell _ ->
            let
                bO =
                    boolOf propIsHoriz cell
            in
            if bO then
                viewHorizStackCell cell

            else
                viewVertStackCell cell

        EffectCell _ ->
            text ""


viewVertStackCell : Node (Cell a) -> Html (Msg a)
viewVertStackCell cell =
    case isaOf cell of
        ContentCell _ ->
            div
                ([ HtmlA.id (pathAsIdFromNode cell)
                 , HtmlA.style "display" "table"
                 ]
                    ++ marginsAndPaddings cell
                )
            <|
                viewCell cell

        EffectCell _ ->
            text ""


viewHorizStackCell : Node (Cell a) -> Html (Msg a)
viewHorizStackCell cell =
    case isaOf cell of
        ContentCell _ ->
            let
                displayAttrs =
                    divRowAttributes cell
            in
            div
                (HtmlA.id (pathAsIdFromNode cell)
                    :: marginsAndPaddings cell
                    ++ (if displayAttrs == [] then
                            [ HtmlA.style "display" "flex" ]

                        else
                            displayAttrs
                       )
                )
            <|
                viewCell cell

        EffectCell _ ->
            text ""


viewConstantCell : Node (Cell a) -> Html (Msg a)
viewConstantCell cell =
    case isaOf cell of
        ContentCell _ ->
            div
                (divCellAttributes cell)
                [ label
                    ([ HtmlA.id (pathAsIdFromNode cell)
                     , HtmlA.style "font-weight" "bold"
                     , HtmlA.style "color" "darkblue"
                     ]
                        ++ marginsAndPaddings cell
                    )
                    [ text (textOf propConstant cell) ]
                ]

        EffectCell _ ->
            text ""


viewInputCell : Node (Cell a) -> Html (Msg a)
viewInputCell cell =
    case isaOf cell of
        ContentCell _ ->
            let
                inputValue =
                    textOf propInput cell

                inputSize =
                    if inputValue == "" then
                        String.length "<no value>"

                    else
                        String.length inputValue
            in
            div
                (divCellAttributes cell)
                [ input
                    ([ HtmlA.style "border-width" "0px"
                     , HtmlA.style "font-family" "Consolas"
                     , HtmlA.style "font-size" "16px"
                     , HtmlA.style "border" "none"
                     , HtmlA.style "outline" "none"
                     , HtmlA.placeholder "<no value>"
                     , HtmlA.value inputValue
                     , HtmlA.size inputSize
                     , HtmlA.id (pathAsIdFromNode cell)
                     ]
                        ++ marginsAndPaddings cell
                        ++ inputCellAttributesFromEffects cell
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
                placeholderValue =
                    textOf propPlaceholder cell

                inputValue =
                    "<"
                        ++ (if placeholderValue == "" then
                                "..."

                            else
                                placeholderValue
                                    ++ ">"
                           )

                inputSize =
                    String.length inputValue
            in
            div []
                [ input
                    ([ HtmlA.style "border-width" "0px"
                     , HtmlA.style "font-family" "Consolas"
                     , HtmlA.style "font-size" "16px"
                     , HtmlA.style "border" "none"
                     , HtmlA.style "outline" "none"
                     , HtmlA.style "color" "#888888"
                     , HtmlA.style "font-style" "italic"
                     , HtmlA.value inputValue
                     , HtmlA.size inputSize
                     , HtmlE.onInput Swallow
                     , HtmlA.id (pathAsIdFromNode cell)
                     ]
                        ++ inputCellAttributesFromEffects cell
                        ++ marginsAndPaddings cell
                    )
                    []
                ]

        EffectCell _ ->
            text ""


viewButtonCell : Node (Cell a) -> Html (Msg a)
viewButtonCell cell =
    case isaOf cell of
        ContentCell _ ->
            let
                onClick =
                    isasUnderCustom featEffects cell
                        |> List.head
                        |> Maybe.andThen
                            (\e ->
                                case e of
                                    EffectCell effect ->
                                        Just [ HtmlE.onClick (OnClick effect cell) ]

                                    _ ->
                                        Nothing
                            )
                        |> Maybe.withDefault []
            in
            button (marginsAndPaddings cell ++ onClick) [ text (textOf propText cell) ]

        EffectCell _ ->
            text ""


viewRefCell : Node (Cell a) -> Html (Msg a)
viewRefCell cell =
    case isaOf cell of
        ContentCell _ ->
            let
                options =
                    getUnderCustom featScope cell
                        |> List.map optionFromScope

                inputId =
                    pathAsIdFromNode cell

                datalistId =
                    inputId ++ "-datalist"

                inputValue =
                    textOf propInput cell

                inputSize =
                    if inputValue == "" then
                        String.length "<no value>" + 2

                    else
                        String.length inputValue + 2
            in
            div
                (divCellAttributes cell)
                [ datalist
                    [ HtmlA.id datalistId
                    ]
                    options
                , input
                    ([ HtmlA.style "border-width" "0px"
                     , HtmlA.style "font-family" "Consolas"
                     , HtmlA.style "font-size" "16px"
                     , HtmlA.style "border" "none"
                     , HtmlA.style "outline" "none"
                     , HtmlA.placeholder "<no value>"
                     , HtmlA.size inputSize
                     , HtmlA.id inputId
                     , HtmlA.list datalistId
                     , HtmlA.value inputValue
                     ]
                        ++ marginsAndPaddings cell
                        ++ inputCellAttributesFromEffects cell
                    )
                    []
                ]

        EffectCell _ ->
            text ""


viewGraphCell : Node (Cell a) -> Html (Msg a)
viewGraphCell cellGraph =
    svg
        [ HtmlA.style "width" "100%"
        , HtmlA.style "height" "100%"
        , HtmlA.style "background-color" "azure"
        ]
        [ g [] <|
            viewEdgeCells cellGraph
        , g [] <|
            List.map viewVertexCell <|
                nodesOf (ContentCell VertexCell) cellGraph
        ]


type alias Entity a =
    Force.Entity Int { value : Node (Cell a) }


initialRadius : Float
initialRadius =
    10


initialAngle : Float
initialAngle =
    pi * (3 - sqrt 5)


forceEntityFromVertex : Int -> Node (Cell a) -> Force.Entity String { value : Node (Cell a) }
forceEntityFromVertex index cell =
    let
        mbX =
            tryFloatOf propX cell

        mbY =
            tryFloatOf propY cell

        radius =
            sqrt (toFloat index) * initialRadius

        angle =
            toFloat index * initialAngle

        xNew =
            case mbX of
                Nothing ->
                    radius * cos angle

                Just xc ->
                    xc

        yNew =
            case mbY of
                Nothing ->
                    radius * sin angle

                Just yc ->
                    yc
    in
    { x = xNew
    , y = yNew
    , vx = 0.0
    , vy = 0.0
    , id = pathAsIdFromNode cell
    , value = cell
    }


dictNameToVertex : Node (Cell a) -> Dict.Dict String (Node (Cell a))
dictNameToVertex cellGraph =
    nodesOf (ContentCell VertexCell) cellGraph
        |> List.foldl (\v d -> Dict.insert (textOf propText v) v d) Dict.empty


edgeForcesFromGraph : Node (Cell a) -> List ( String, String )
edgeForcesFromGraph cellGraph =
    let
        edges =
            nodesOf (ContentCell EdgeCell) cellGraph

        idLookup edge =
            let
                ( from, to ) =
                    ( textOf propFrom edge, textOf propTo edge )

                lookupWithDefault key =
                    Dict.get key (dictNameToVertex cellGraph)
                        |> Maybe.andThen (\v -> Just <| pathAsIdFromNode v)
                        |> Maybe.withDefault ""
            in
            ( lookupWithDefault from, lookupWithDefault to )
    in
    List.map idLookup edges

customEdgeForcesFromGraph : Node (Cell a) -> List { source : String, target : String, distance : Float, strength : Maybe Float }
customEdgeForcesFromGraph cellGraph =
    let
        edges =
            nodesOf (ContentCell EdgeCell) cellGraph

        forceLookup edge =
            let
                ( from, to ) =
                    ( textOf propFrom edge, textOf propTo edge )

                lookupWithDefault key =
                    Dict.get key (dictNameToVertex cellGraph)
                        |> Maybe.andThen (\v -> Just <| pathAsIdFromNode v)
                        |> Maybe.withDefault ""
            in
            { source = lookupWithDefault from
            , target = lookupWithDefault to
            , distance = 150
            , strength = Nothing
            }
    in
    List.map forceLookup edges


viewEdgeCells : Node (Cell a) -> List (Html (Msg a))
viewEdgeCells cellGraph =
    let
        edges =
            nodesOf (ContentCell EdgeCell) cellGraph

        fromToLookup edge =
            let
                ( from, to ) =
                    ( textOf propFrom edge, textOf propTo edge )

                lookup key =
                    Dict.get key (dictNameToVertex cellGraph)
            in
            ( lookup from, lookup to )

        fromToPairs =
            List.map fromToLookup edges
    in
    List.map viewEdgeCell fromToPairs


viewEdgeCell : ( Maybe (Node (Cell a)), Maybe (Node (Cell a)) ) -> Html (Msg a)
viewEdgeCell fromTo =
    case fromTo of
        ( Nothing, _ ) ->
            text ""

        ( _, Nothing ) ->
            text ""

        ( Just from, Just to ) ->
            line
                [ strokeWidth 1
                , stroke <| Paint <| Color.rgb255 170 170 170
                , x1 <| floatOf propX from
                , y1 <| floatOf propY from
                , x2 <| floatOf propX to
                , y2 <| floatOf propY to
                ]
                []


viewVertexCell : Node (Cell a) -> Html (Msg a)
viewVertexCell cell =
    let
        name =
            tryTextOf propText cell |> Maybe.withDefault "<no name>"

        nameNotEmpty = if name == "" then "<no name>" else name

        wRect =
            (toFloat <| String.length <| nameNotEmpty) * 10

        hRect =
            40

        xPos =
            floatOf propX cell

        xPosMiddle =
            xPos - (wRect / 2)

        yPos =
            floatOf propY cell

        yPosMiddle =
            yPos - (hRect / 2)
    in
    g []
        [ rect
            [ width wRect
            , height hRect
            , fill <| Paint <| Color.rgb255 155 173 255
            , stroke <| Paint <| Color.blue
            , strokeWidth 2

            --, onMouseDown cell.id
            , x xPosMiddle
            , y yPosMiddle
            , rx 4
            , ry 4
            ]
            []
        , text_ [ x xPos, y yPos, textAnchor AnchorMiddle ] [ text nameNotEmpty ]
        ]


optionFromScope : Node (Cell a) -> Html (Msg a)
optionFromScope scopeElement =
    let
        scopeValue =
            textOf propConstant scopeElement
    in
    option
        [ HtmlA.value scopeValue ]
        []


divRowAttributes : Node (Cell a) -> List (Attribute (Msg a))
divRowAttributes cell =
    if boolOf propIsGrid cell then
        [ HtmlA.style "display" "table-row" ]

    else
        []


divCellAttributes : Node (Cell a) -> List (Attribute (Msg a))
divCellAttributes cell =
    if boolOf propIsGrid cell then
        [ HtmlA.style "display" "table-cell" ]

    else
        []


marginsAndPaddings : Node (Cell a) -> List (Attribute (Msg a))
marginsAndPaddings cell =
    [ margins cell, paddings cell ]


margins : Node (Cell a) -> Attribute (Msg a)
margins cell =
    let
        indentMarginLeft =
            if boolOf propIndent cell then
                20

            else
                0

        top =
            (intOf propMarginTop cell |> String.fromInt) ++ "px "

        right =
            ((intOf propMarginRight cell + 5) |> String.fromInt) ++ "px "

        bottom =
            (intOf propMarginBottom cell |> String.fromInt) ++ "px "

        left =
            ((intOf propMarginLeft cell + indentMarginLeft) |> String.fromInt) ++ "px"
    in
    HtmlA.style "margin" <| top ++ right ++ bottom ++ left


paddings : Node (Cell a) -> Attribute (Msg a)
paddings _ =
    HtmlA.style "padding" <| "0px 0px 0px 0px"


inputCellAttributesFromEffects : Node (Cell a) -> List (Attribute (Msg a))
inputCellAttributesFromEffects cell =
    let
        effectGroups =
            grouped <|
                isasUnderCustom featEffects cell
                    ++ navEffects (pathOf cell)
    in
    List.map (attributeFromEffectGroup cell) effectGroups
        |> List.filterMap identity


attributeFromEffectGroup : Node (Cell a) -> EffectGroup a -> Maybe (Attribute (Msg a))
attributeFromEffectGroup cell effectGroup =
    case effectGroup of
        InputEffectGroup effects ->
            case effects of
                effect :: [] ->
                    Just (effectAttributeFromInput (OnInput effect))

                _ ->
                    Nothing

        KeyboardEffectGroup effects ->
            Just (effectAttributeFromKey (inputEffectMap cell effects))

        FocusEffectGroup effects ->
            case effects of
                effect :: [] ->
                    Just (effectAttributeFromFocus (UpdateScope effect))

                _ ->
                    Nothing


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


effectAttributeFromFocus : Msg a -> Attribute (Msg a)
effectAttributeFromFocus msg =
    HtmlE.onFocus msg


inputEffectMap : Node (Cell a) -> List (EffectCell a) -> Dict.Dict String (Msg a)
inputEffectMap cell effects =
    List.foldl
        (\effect dict ->
            case effect of
                InsertionEffect _ ->
                    Dict.insert "Enter" (OnEnter effect cell) dict

                DeletionEffect _ ->
                    Dict.insert "Delete" (OnDelete effect cell) <|
                        Dict.insert "Backspace" (OnBackspace effect cell) dict

                NavSelectionEffect { dir } ->
                    Dict.insert (keyFromDir dir) (NavSelection effect) dict

                InputEffect _ ->
                    dict

                CreateScopeEffect _ ->
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
                    JsonD.fail <| "incorrect code: " ++ k

                Just msg ->
                    case msg of
                        NavSelection effect ->
                            case effect of
                                NavSelectionEffect navData ->
                                    JsonD.map
                                        (\sel ->
                                            NavSelection <| NavSelectionEffect { navData | selection = sel }
                                        )
                                        (JsonD.field "target" decodeSelection)

                                _ ->
                                    JsonD.succeed msg

                        OnDelete effect cellContext ->
                            case effect of
                                DeletionEffect effectData ->
                                    JsonD.map
                                        (\sel ->
                                            OnDelete (DeletionEffect { effectData | selection = sel }) cellContext
                                        )
                                        (JsonD.field "target" decodeSelection)

                                _ ->
                                    JsonD.succeed msg

                        OnBackspace effect cellContext ->
                            case effect of
                                DeletionEffect effectData ->
                                    JsonD.map
                                        (\sel ->
                                            OnBackspace (DeletionEffect { effectData | selection = sel }) cellContext
                                        )
                                        (JsonD.field "target" decodeSelection)

                                _ ->
                                    JsonD.succeed msg

                        _ ->
                            JsonD.succeed msg
    in
    HtmlE.on "keydown" <|
        JsonD.andThen canHandle <|
            JsonD.field "key" JsonD.string


isasUnderCustom : String -> Node a -> List a
isasUnderCustom featureKey parent =
    let
        children =
            if featureKey == "" then
                getUnderDefault parent

            else
                getUnderCustom featureKey parent
    in
    List.map isaOf children


orientationOf : Node (Cell a) -> Node (Cell a) -> Maybe Orientation
orientationOf root cell =
    case isaOf cell of
        ContentCell StackCell ->
            Just <|
                let
                    bO =
                        boolOf propIsHoriz cell
                in
                if bO then
                    Horiz

                else
                    Vert

        ContentCell _ ->
            parentOf root (pathOf cell)
                |> Maybe.andThen (orientationOf root)

        _ ->
            Nothing


decodeSelection : JsonD.Decoder Selection
decodeSelection =
    JsonD.map3
        (\s e d ->
            { start = s
            , end = e
            , dir = d
            }
        )
        (JsonD.field "selectionStart" JsonD.int)
        (JsonD.field "selectionEnd" JsonD.int)
        (JsonD.field "selectionDirection" JsonD.string)


griddify : Node (Cell a) -> Node (Cell a)
griddify =
    griddifyI False


griddifyI : Bool -> Node (Cell a) -> Node (Cell a)
griddifyI isGridParent node =
    let
        nodeNew =
            if isGridParent then
                addBool propIsGrid True node

            else
                node

        children =
            getUnderDefault nodeNew

        isGrid =
            boolOf propIsGrid nodeNew
    in
    replaceUnderFeature featDefault (List.map (griddifyI isGrid) children) nodeNew



-- CUSTOM FEATURE AND PROPERTY CONSTANTS


featDefault =
    "default"


featScope =
    "scope"


featEffects =
    "effects"


propScopeValue =
    "scopeValue"


propName =
    "name"


propInput =
    "input"


propConstant =
    "constant"


propIsHoriz =
    "isHoriz"


propPlaceholder =
    "placeholder"


propIsGrid =
    "isGrid"


propText =
    "text"


propX =
    "x"


propY =
    "y"


propFrom =
    "propFrom"


propTo =
    "propTo"


propIndent =
    "indent"


propMarginTop =
    "margin-top"


propMarginBottom =
    "margin-bottom"


propMarginRight =
    "margin-right"


propMarginLeft =
    "margin-left"
