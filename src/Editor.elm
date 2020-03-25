module Editor exposing
    ( Cell
    , EditorModel
    , MarginSide(..)
    , MousePosition
    , Msg(..)
    , addIndent
    , addMargin
    , buttonCell
    , constantCell
    , deletionEffect
    , edgeCell
    , graphCell
    , graphComparer
    , griddify
    , horizSplitCell
    , horizStackCell
    , initEditorModel
    , inputCell
    , inputEffect
    , insertionEffect
    , mousePosition
    , persistVertexPositions
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
import Color exposing (Color)
import Dict as Dict exposing (Dict)
import Direction2d as D2d exposing (Direction2d)
import Force exposing (Entity, Force, State)
import Geometry.Svg as GSvg
import Html exposing (..)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD
import LineSegment2d as LS2d exposing (LineSegment2d)
import Point2d as P2d exposing (Point2d)
import Structure exposing (..)
import Task as Task
import Triangle2d as T2d exposing (Triangle2d)
import TypedSvg exposing (circle, g, line, rect, svg)
import TypedSvg.Attributes exposing (fill, stroke)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, rx, ry, strokeWidth, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (foreignObject)
import TypedSvg.Events as TsvgE
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..))
import Vector2d as V2d exposing (Vector2d)


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
    , drag : Maybe Drag
    , mousePos : Point2d
    , runXform : Bool
    , runSimulation : Bool
    }


type alias Graph =
    { mbSimulation : Maybe (Force.State String)
    , drag : Maybe Drag
    , runSimulation : Bool
    }


type alias Drag =
    { mousePosStart : Point2d
    , vertexPosStart : Point2d
    , path : Path
    }


type alias InsertionEffectData a =
    { path : Path
    , nodeToInsert : Node a
    , isReplace : Bool
    , role : Role
    }


type alias DeletionEffectData =
    { path : Path
    , selection : Selection
    }


type alias InputEffectData =
    { path : Path
    , role : Role
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
    | DragStart Path
    | MouseMove Point2d
    | MouseUp Point2d


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


type alias MousePosition =
    Point2d


type alias Entity a =
    Force.Entity Int { value : Node (Cell a) }


type alias VertexProperties =
    { posVertex : Point2d
    , posContent : Point2d
    , widthVertex : Float
    , heightVertex : Float
    , angleAreas : List ( Float, Float )
    , widthContent : Int
    , heightContent : Float
    , content : String
    }



-- CREATION


initEditorModel : Node a -> Node (Cell a) -> EditorModel a
initEditorModel dRoot eRoot =
    { dRoot = dRoot
    , eRoot = eRoot
    , mbSimulation = Nothing
    , drag = Nothing
    , mousePos = P2d.origin
    , runXform = True
    , runSimulation = True
    }


rootCell : Node (Cell a)
rootCell =
    createRoot (ContentCell RootCell)


constantCell : String -> Node (Cell a)
constantCell constantValue =
    createNode (ContentCell ConstantCell)
        |> addText roleConstant constantValue


refCell : a -> Role -> Node a -> Maybe (List String) -> Node (Cell a)
refCell target role nodeContext scopeProvider =
    let
        pathContext =
            pathOf nodeContext
    in
    createNode (ContentCell RefCell)
        |> addText roleInput (textOf role nodeContext)
        |> addRangeToCustom roleScope (createRefScope nodeContext)
        |> withEffect (inputEffect pathContext role)
        |> withEffect (createScopeEffect target (pathOf nodeContext) scopeProvider)


createRefScope nodeContext =
    let
        scopeCell scopeElement =
            constantCell (textOf roleScopeValue scopeElement)
    in
    getUnderCustom roleScope nodeContext
        |> List.map scopeCell


inputCell : Role -> Node a -> Node (Cell a)
inputCell role nodeContext =
    createNode (ContentCell InputCell)
        |> addText roleInput (textOf role nodeContext)
        |> withEffect (inputEffect (pathOf nodeContext) role)


horizStackCell : Node (Cell a)
horizStackCell =
    createNode (ContentCell StackCell)
        |> addBool roleIsHoriz True


vertStackCell : Node (Cell a)
vertStackCell =
    createNode (ContentCell StackCell)
        |> addBool roleIsHoriz False


vertSplitCell : Node (Cell a)
vertSplitCell =
    createNode (ContentCell SplitCell)


horizSplitCell : Node (Cell a)
horizSplitCell =
    createNode (ContentCell SplitCell)
        |> addBool roleIsHoriz True


vertGridCell : Node (Cell a)
vertGridCell =
    vertStackCell
        |> addBool roleIsGrid True


placeholderCell : String -> Node (Cell a)
placeholderCell text =
    createNode (ContentCell PlaceholderCell)
        |> addText rolePlaceholder text


buttonCell : String -> Node (Cell a)
buttonCell text =
    createNode (ContentCell ButtonCell)
        |> addText roleText text


graphCell : Node (Cell a)
graphCell =
    createNode (ContentCell GraphCell)


vertexCell : Role -> Node a -> Node (Cell a)
vertexCell role nodeContext =
    createNode (ContentCell VertexCell)
        |> addText roleText (textOf role nodeContext)
        |> withEffect (inputEffect (pathOf nodeContext) role)


edgeCell : Role -> ( String, String ) -> Node a -> Node (Cell a)
edgeCell role ( from, to ) nodeContext =
    createNode (ContentCell EdgeCell)
        |> addText roleText (textOf role nodeContext)
        |> addText roleFrom from
        |> addText roleTo to


with : Node (Cell a) -> Node (Cell a) -> Node (Cell a)
with node =
    addToDefault node


withRange : List (Node (Cell a)) -> Node (Cell a) -> Node (Cell a)
withRange children =
    addRangeToDefault children


addIndent : Node (Cell a) -> Node (Cell a)
addIndent node =
    addBool roleIndent True node


addMargin : MarginSide -> Int -> Node (Cell a) -> Node (Cell a)
addMargin side space node =
    let
        key =
            case side of
                Top ->
                    roleMarginTop

                Right ->
                    roleMarginRight

                Bottom ->
                    roleMarginBottom

                Left ->
                    roleMarginLeft
    in
    addInt key space node



-- EFFECTS


withEffect : EffectCell a -> Node (Cell a) -> Node (Cell a)
withEffect effect =
    addToCustom roleEffects <|
        createNode <|
            EffectCell effect


replacementEffect : Role -> Node a -> Node a -> EffectCell a
replacementEffect role nodeContext nodeToInsert =
    InsertionEffect <|
        InsertionEffectData (pathOf nodeContext) nodeToInsert True role


insertionEffect : Node a -> Node a -> EffectCell a
insertionEffect nodeContext nodeToInsert =
    InsertionEffect <|
        InsertionEffectData (pathOf nodeContext) nodeToInsert False roleEmpty


deletionEffect : Node a -> EffectCell a
deletionEffect nodeContext =
    DeletionEffect <|
        DeletionEffectData (pathOf nodeContext) emptySelection


inputEffect : Path -> Role -> EffectCell a
inputEffect path role =
    InputEffect <| InputEffectData path role


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


updateEditor : Msg a -> EditorModel a -> ( EditorModel a, Cmd (Msg a) )
updateEditor msg editorModel =
    case msg of
        Tick ->
            tickGraphSimulations editorModel

        DragStart path ->
            let
                mbVertex =
                    nodeAt editorModel.eRoot path
            in
            case mbVertex of
                Nothing ->
                    noUpdate editorModel

                Just vertex ->
                    let
                        vertextPosStart =
                            p2dFromCell vertex

                        vertexGrabbed =
                            vertex |> addBool roleGrabbed True

                        eRootNew =
                            replaceChildAtPath vertexGrabbed (pathOf vertex) editorModel.eRoot
                    in
                    ( { editorModel
                        | drag = Just <| Drag editorModel.mousePos vertextPosStart path
                        , eRoot = eRootNew
                        , runXform = False
                      }
                    , Cmd.none
                    )

        MouseMove mousePosNew ->
            case editorModel.drag of
                Just drag ->
                    let
                        mbSimNew =
                            editorModel.mbSimulation
                                |> Maybe.andThen (\s -> Just <| Force.reheat s)
                    in
                    ( { editorModel
                        | mbSimulation = mbSimNew
                        , eRoot = updateDrag editorModel.eRoot drag mousePosNew
                        , mousePos = mousePosNew
                        , runXform = False
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { editorModel
                        | mousePos = mousePosNew
                        , runXform = False
                        , runSimulation = False
                      }
                    , Cmd.none
                    )

        MouseUp mousePosNew ->
            case editorModel.drag of
                Just drag ->
                    let
                        mbVertex =
                            nodeAt editorModel.eRoot drag.path
                    in
                    case mbVertex of
                        Nothing ->
                            noUpdate editorModel

                        Just vertex ->
                            let
                                vertexGrabbed =
                                    vertex |> addBool roleGrabbed False

                                eRootNew =
                                    replaceChildAtPath vertexGrabbed (pathOf vertex) editorModel.eRoot
                            in
                            ( { editorModel
                                | eRoot = updateDrag eRootNew drag mousePosNew
                                , drag = Nothing
                                , runXform = False
                              }
                            , Cmd.none
                            )

                Nothing ->
                    noUpdate editorModel

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
            ( { editorModel | runXform = False }, updateOnNavEffect effect editorModel.eRoot )

        UpdateScope effect ->
            updateOnCreateScopeEffect editorModel effect


tickGraphSimulations : EditorModel a -> ( EditorModel a, Cmd (Msg a) )
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
                            , Force.manyBodyStrength -500 <| List.map (\v -> pathAsIdFromNode v) <| verticies
                            , Force.center 400 300
                            ]
                    in
                    ( { editorModel | mbSimulation = Just <| Force.simulation forces, runXform = False }, Cmd.none )

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
                                |> addFloat roleX e.x
                                |> addFloat roleY e.y

                        entities =
                            List.indexedMap (\i v -> forceEntityFromVertex i v) verticies

                        --d2 = Debug.log "Input Force.tick" "(x, y) (vx, vy)"
                        --d = List.map (\e -> ((e.x, e.y), (e.vx, e.vy))) entities |> Debug.log ""
                        ( newSimulationState, entitiesNew ) =
                            entities
                                |> Force.tick simulation

                        -- |> Debug.log "sim")
                        --o2 = Debug.log "Output Force.tick" "(x, y) (vx, vy)"
                        --o = List.map (\e -> ((e.x, e.y), (e.vx, e.vy))) entities |> Debug.log ""
                        childrenNew =
                            List.map addPosToCell entitiesNew
                                ++ edges

                        cellGraphNew =
                            replaceUnderFeature roleDefault childrenNew cellGraph

                        eRootNew =
                            replaceChildAtPath cellGraphNew pathToGraph editorModel.eRoot

                        eRootWithDrag =
                            case editorModel.drag of
                                Nothing ->
                                    eRootNew

                                Just drag ->
                                    updateDrag eRootNew drag editorModel.mousePos
                    in
                    ( { editorModel | eRoot = eRootWithDrag, mbSimulation = Just <| newSimulationState, runXform = False }, Cmd.none )


updateDrag : Node (Cell a) -> Drag -> Point2d -> Node (Cell a)
updateDrag eRoot drag mousePosCurrent =
    let
        delta =
            V2d.from drag.mousePosStart mousePosCurrent

        ( xNew, yNew ) =
            drag.vertexPosStart |> P2d.translateBy delta |> P2d.coordinates

        eRootTemp =
            updatePropertyByPath eRoot drag.path ( roleX, asPFloat xNew )
    in
    updatePropertyByPath eRootTemp drag.path ( roleY, asPFloat yNew )


updateOnInsertionEffect : EditorModel a -> EffectCell a -> Node (Cell a) -> ( EditorModel a, Cmd (Msg a) )
updateOnInsertionEffect editorModel effect cellContext =
    case effect of
        InsertionEffect { path, nodeToInsert, isReplace, role } ->
            if isReplace then
                let
                    dRootNew =
                        addChildAtPath role nodeToInsert path editorModel.dRoot |> updatePaths
                in
                ( { editorModel | dRoot = dRootNew, runXform = True }, Cmd.none )

            else
                let
                    dRootNew =
                        insertChildAfterPath nodeToInsert path editorModel.dRoot |> updatePaths
                in
                ( { editorModel | dRoot = dRootNew, runXform = True }, updateSelectionOnEnter cellContext )

        _ ->
            noUpdate editorModel


updateOnDeleteEffect : EditorModel a -> EffectCell a -> Node (Cell a) -> ( EditorModel a, Cmd (Msg a) )
updateOnDeleteEffect editorModel effect cellContext =
    case effect of
        DeletionEffect ({ selection } as effectData) ->
            let
                textLength =
                    textOf roleInput cellContext |> String.length

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


updateOnBackspaceEffect : EditorModel a -> EffectCell a -> Node (Cell a) -> ( EditorModel a, Cmd (Msg a) )
updateOnBackspaceEffect editorModel effect cellContext =
    case effect of
        DeletionEffect ({ selection } as effectData) ->
            let
                textLength =
                    textOf roleInput cellContext |> String.length

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


updateOnInputEffect : EditorModel a -> EffectCell a -> String -> ( EditorModel a, Cmd (Msg a) )
updateOnInputEffect editorModel effect value =
    case effect of
        InputEffect { path, role } ->
            ( { editorModel
                | dRoot = updatePropertyByPath editorModel.dRoot path ( role, asPString value ) |> updatePaths
                , runXform = True
              }
            , Cmd.none
            )

        _ ->
            noUpdate editorModel


updateOnNavEffect : EffectCell a -> Node (Cell a) -> Cmd (Msg a)
updateOnNavEffect effect editorModel =
    case effect of
        NavSelectionEffect navData ->
            updateSelection editorModel navData

        _ ->
            Cmd.none


updateOnCreateScopeEffect : EditorModel a -> EffectCell a -> ( EditorModel a, Cmd (Msg a) )
updateOnCreateScopeEffect editorModel effect =
    case effect of
        CreateScopeEffect scopeData ->
            ( { editorModel | dRoot = setScopeInformation editorModel.dRoot scopeData, runXform = True }, Cmd.none )

        _ ->
            noUpdate editorModel


noUpdate editorModel =
    ( { editorModel | runXform = False }, Cmd.none )


setScopeInformation : Node a -> CreateScopeEffectData a -> Node a
setScopeInformation domainModel scopeData =
    let
        optionNodes =
            nodesOf scopeData.isa domainModel
                |> List.map (\s -> createNode scopeData.isa |> addText roleScopeValue (textOf roleName s))
    in
    replaceRangeAtPath
        roleScope
        optionNodes
        scopeData.pathContextNode
        domainModel


tryDelete : EditorModel a -> DeletionEffectData -> (Node a -> Path -> Maybe (Node a)) -> Int -> Bool -> ( EditorModel a, Cmd (Msg a) )
tryDelete editorModel { path } navFun textLength isAtDeletePos =
    if textLength == 0 then
        ( { editorModel | dRoot = deleteNodeUnder path editorModel.dRoot |> updatePaths, runXform = True }, Cmd.none )

    else if isAtDeletePos then
        let
            mbNext =
                navFun editorModel.dRoot path
        in
        case mbNext of
            Nothing ->
                noUpdate editorModel

            Just next ->
                ( { editorModel | dRoot = deleteNodeUnder (pathOf next) editorModel.dRoot |> updatePaths, runXform = True }, Cmd.none )

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
                    if navData.selection.start >= (textOf roleInput cellSelected |> String.length) then
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
                    boolOf roleIsHoriz cell
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
                            ( viewContent first []
                            , viewContent second []
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
                    boolOf roleIsHoriz cell
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
                    [ text (textOf roleConstant cell) ]
                ]

        EffectCell _ ->
            text ""


viewInputCell : Node (Cell a) -> Html (Msg a)
viewInputCell cell =
    case isaOf cell of
        ContentCell _ ->
            let
                inputValue =
                    textOf roleInput cell

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
                    textOf rolePlaceholder cell

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
                    isasUnderCustom roleEffects cell
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
            button (marginsAndPaddings cell ++ onClick) [ text (textOf roleText cell) ]

        EffectCell _ ->
            text ""


viewRefCell : Node (Cell a) -> Html (Msg a)
viewRefCell cell =
    case isaOf cell of
        ContentCell _ ->
            let
                options =
                    getUnderCustom roleScope cell
                        |> List.map optionFromScope

                inputId =
                    pathAsIdFromNode cell

                datalistId =
                    inputId ++ "-datalist"

                inputValue =
                    textOf roleInput cell

                inputSize =
                    if inputValue == "" then
                        String.length "<no target>" + 2

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
                     , HtmlA.placeholder "<no target>"
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
        , HtmlA.style "height" "800px"
        , HtmlA.style "background-color" "AliceBlue"
        ]
        [ g [] <|
            viewEdgeCells cellGraph
        , g [] <|
            List.map viewVertexCell <|
                nodesOf (ContentCell VertexCell) cellGraph
        ]


viewEdgeCells : Node (Cell a) -> List (Html (Msg a))
viewEdgeCells cellGraph =
    List.map viewEdgeCell (fromToPairs cellGraph)


viewEdgeCell : ( Node (Cell a), Node (Cell a) ) -> Html (Msg a)
viewEdgeCell fromTo =
    let
        ( edgeStart, edgeEnd ) =
            vertexAnchorsForEdge fromTo

        edgeLine =
            LS2d.from edgeStart edgeEnd
    in
    edgeWithArrowHead edgeLine


vertexAnchorsForEdge : ( Node (Cell a), Node (Cell a) ) -> ( Point2d, Point2d )
vertexAnchorsForEdge ( from, to ) =
    let
        fProps =
            vertexProperties from

        tProps =
            vertexProperties to

        fSector =
            sectorFromAngle fProps.angleAreas

        tSector =
            sectorFromAngle tProps.angleAreas

        fPos =
            p2dFromCell from

        tPos =
            p2dFromCell to

        dir =
            D2d.from fPos tPos |> Maybe.withDefault D2d.positiveX

        angle =
            D2d.toAngle dir

        sectorFromAngle a =
            a
                |> List.indexedMap
                    (\i ( lowest, highest ) ->
                        if (pi + angle) >= lowest && (pi + angle) < highest then
                            Just i

                        else
                            Nothing
                    )
                |> List.filterMap identity
                |> List.head
                |> Maybe.withDefault 0

        translate s pos props inv =
            case s of
                0 ->
                    P2d.translateIn dir (inv * (-props.widthVertex / 2) / cos angle) pos

                1 ->
                    P2d.translateIn dir (inv * (-props.heightVertex / 2) / cos ((pi / 2) - angle)) pos

                2 ->
                    P2d.translateIn dir (inv * (props.widthVertex / 2) / cos angle) pos

                3 ->
                    P2d.translateIn dir (inv * (props.heightVertex / 2) / cos ((pi / 2) - angle)) pos

                _ ->
                    pos
    in
    ( translate fSector fPos fProps 1, translate tSector tPos tProps -1 )


vertexProperties : Node (Cell a) -> VertexProperties
vertexProperties vertex =
    let
        noName =
            "<no value>"

        name =
            tryTextOf roleText vertex |> Maybe.withDefault noName

        nameContent =
            if name == "" then
                noName

            else
                name

        widthVertex =
            (toFloat <| String.length <| nameContent) * 8.797 + 18

        heightVertex =
            40

        halfH =
            heightVertex / 2

        halfW =
            widthVertex / 2

        -- it's easier to work with 0..2pi than -pi to pi
        edgeAngle =
            atan2 halfH halfW

        angleAreas =
            [ ( 2 * pi - edgeAngle, edgeAngle )
            , ( edgeAngle, pi - edgeAngle )
            , ( pi - edgeAngle, pi + edgeAngle )
            , ( pi + edgeAngle, 2 * pi - edgeAngle )
            ]

        midTranslation =
            V2d.fromComponents ( -halfW, -halfH )

        posVertex =
            p2dFromCell vertex
                |> P2d.translateBy midTranslation

        posContent =
            posVertex
                |> P2d.translateBy (V2d.fromComponents ( 9, 9 ))

        widthContent =
            if nameContent == "" then
                String.length noName

            else
                String.length nameContent
    in
    { posVertex = posVertex
    , posContent = posContent
    , widthVertex = widthVertex
    , heightVertex = heightVertex
    , angleAreas = angleAreas
    , widthContent = widthContent
    , heightContent = 20
    , content = nameContent
    }


viewVertexCell : Node (Cell a) -> Html (Msg a)
viewVertexCell cell =
    let
        vertexProps =
            vertexProperties cell

        handle =
            vertexDragHandle cell vertexProps

        content =
            vertexContent cell vertexProps
    in
    g []
        [ rect
            [ width vertexProps.widthVertex
            , height vertexProps.heightVertex
            , fill <| Paint <| Color.white
            , stroke <| Paint <| colorGraphPrimary
            , strokeWidth 2
            , x <| P2d.xCoordinate vertexProps.posVertex
            , y <| P2d.yCoordinate vertexProps.posVertex
            , rx 4
            , ry 4
            ]
            []
        , content
        , handle
        ]


vertexContent : Node (Cell a) -> VertexProperties -> Html (Msg a)
vertexContent cell { posContent, widthVertex, widthContent, heightContent, content } =
    foreignObject
        [ x <| P2d.xCoordinate posContent
        , y <| P2d.yCoordinate posContent
        , width widthVertex
        , height heightContent
        ]
        [ form []
            [ input
                ([ HtmlA.style "border-width" "0px"
                 , HtmlA.style "font-family" "Consolas"
                 , HtmlA.style "font-size" "16px"
                 , HtmlA.style "border" "none"
                 , HtmlA.style "outline" "none"
                 , HtmlA.placeholder "<no value>"
                 , HtmlA.value content
                 , HtmlA.size widthContent
                 , HtmlA.style "background-color" "transparent"
                 , HtmlA.disabled <| boolOf roleGrabbed cell
                 ]
                    ++ inputCellAttributesFromEffects cell
                )
                []
            ]
        ]


vertexDragHandle : Node (Cell a) -> VertexProperties -> Html (Msg a)
vertexDragHandle cell { posVertex } =
    if boolOf roleGrabbed cell then
        circle
            [ r 10
            , cx <| P2d.xCoordinate posVertex
            , cy <| P2d.yCoordinate posVertex
            , fill <| Paint <| colorGraphPrimary
            ]
            []

    else
        circle
            [ r 5
            , cx <| P2d.xCoordinate posVertex
            , cy <| P2d.yCoordinate posVertex
            , TsvgE.onMouseDown (DragStart (pathOf cell))
            , stroke <| Paint <| colorGraphPrimary
            , strokeWidth 2
            , fill <| Paint <| Color.rgb255 240 248 255
            ]
            []


edgeWithArrowHead : LS2d.LineSegment2d -> Html (Msg a)
edgeWithArrowHead lineSegment =
    let
        dir =
            LS2d.direction lineSegment
                |> Maybe.withDefault D2d.positiveX

        angle =
            D2d.toAngle dir

        vecFromOriginToEndPoint =
            LS2d.endPoint lineSegment
                |> P2d.coordinates
                |> V2d.fromComponents

        headWidth =
            15

        headLength =
            15

        arrowHead =
            T2d.fromVertices
                ( P2d.fromCoordinates ( 0, -headWidth / 2 )
                , P2d.fromCoordinates ( 0, headWidth / 2 )
                , P2d.fromCoordinates ( headLength, 0 )
                )
                |> T2d.rotateAround P2d.origin angle
                |> T2d.translateBy vecFromOriginToEndPoint
                |> T2d.translateIn dir -headLength
    in
    g []
        [ GSvg.lineSegment2d
            [ stroke <| Paint colorGraphPrimary
            , strokeWidth 2
            ]
            (LS2d.from
                (LS2d.startPoint lineSegment)
                (LS2d.endPoint lineSegment
                    |> P2d.translateIn dir -headLength
                )
            )
        , GSvg.triangle2d
            [ fill <| Paint colorGraphPrimary
            ]
            arrowHead
        ]


optionFromScope : Node (Cell a) -> Html (Msg a)
optionFromScope scopeElement =
    let
        scopeValue =
            textOf roleConstant scopeElement
    in
    option
        [ HtmlA.value scopeValue ]
        []


divRowAttributes : Node (Cell a) -> List (Attribute (Msg a))
divRowAttributes cell =
    if boolOf roleIsGrid cell then
        [ HtmlA.style "display" "table-row" ]

    else
        []


divCellAttributes : Node (Cell a) -> List (Attribute (Msg a))
divCellAttributes cell =
    if boolOf roleIsGrid cell then
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
            if boolOf roleIndent cell then
                20

            else
                0

        top =
            (intOf roleMarginTop cell |> String.fromInt) ++ "px "

        right =
            ((intOf roleMarginRight cell + 5) |> String.fromInt) ++ "px "

        bottom =
            (intOf roleMarginBottom cell |> String.fromInt) ++ "px "

        left =
            ((intOf roleMarginLeft cell + indentMarginLeft) |> String.fromInt) ++ "px"
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
                isasUnderCustom roleEffects cell
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


orientationOf : Node (Cell a) -> Node (Cell a) -> Maybe Orientation
orientationOf root cell =
    case isaOf cell of
        ContentCell StackCell ->
            Just <|
                let
                    bO =
                        boolOf roleIsHoriz cell
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
                addBool roleIsGrid True node

            else
                node

        children =
            getUnderDefault nodeNew

        isGrid =
            boolOf roleIsGrid nodeNew
    in
    replaceUnderFeature roleDefault (List.map (griddifyI isGrid) children) nodeNew


mousePosition : JsonD.Decoder MousePosition
mousePosition =
    JsonD.map2 (\x y -> P2d.fromCoordinates ( x, y ))
        (JsonD.field "clientX" JsonD.float)
        (JsonD.field "clientY" JsonD.float)


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
            tryFloatOf roleX cell

        mbY =
            tryFloatOf roleY cell

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
        |> List.foldl (\v d -> Dict.insert (textOf roleText v) v d) Dict.empty


edgeForcesFromGraph : Node (Cell a) -> List ( String, String )
edgeForcesFromGraph cellGraph =
    let
        edges =
            nodesOf (ContentCell EdgeCell) cellGraph

        idLookup edge =
            let
                ( from, to ) =
                    ( textOf roleFrom edge, textOf roleTo edge )

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
                    ( textOf roleFrom edge, textOf roleTo edge )

                mbLookupWithDefault key =
                    Dict.get key (dictNameToVertex cellGraph)
                        |> Maybe.andThen (\v -> Just <| pathAsIdFromNode v)

                mbSource =
                    mbLookupWithDefault from

                mbTarget =
                    mbLookupWithDefault to
            in
            case ( mbSource, mbTarget ) of
                ( Nothing, _ ) ->
                    Nothing

                ( _, Nothing ) ->
                    Nothing

                ( Just source, Just target ) ->
                    if source == target then
                        Nothing

                    else
                        Just
                            { source = source
                            , target = target
                            , distance = 150
                            , strength = Nothing
                            }
    in
    List.map forceLookup edges
        |> List.filterMap identity


fromToPairs : Node (Cell a) -> List ( Node (Cell a), Node (Cell a) )
fromToPairs cellGraph =
    let
        edges =
            nodesOf (ContentCell EdgeCell) cellGraph

        fromToLookup edge =
            let
                ( from, to ) =
                    ( textOf roleFrom edge, textOf roleTo edge )

                lookup key =
                    Dict.get key (dictNameToVertex cellGraph)
            in
            ( lookup from, lookup to )
    in
    List.map fromToLookup edges
        |> List.filterMap
            (\tuple ->
                case tuple of
                    ( Nothing, _ ) ->
                        Nothing

                    ( _, Nothing ) ->
                        Nothing

                    ( Just from, Just to ) ->
                        Just ( from, to )
            )


persistVertexPositions : Node (Cell a) -> Node (Cell a) -> Node (Cell a)
persistVertexPositions eRootOld eRootNew =
    let
        verticiesOld =
            nodesOf (ContentCell VertexCell) eRootOld

        persistVertexPos vOld rootNew =
            let
                mbx =
                    tryFloatOf roleX vOld

                mby =
                    tryFloatOf roleY vOld
            in
            case ( mbx, mby ) of
                ( Nothing, _ ) ->
                    rootNew

                ( _, Nothing ) ->
                    rootNew

                ( Just x, Just y ) ->
                    let
                        rootNew1 =
                            updatePropertyByPath rootNew (pathOf vOld) ( roleX, asPFloat x )

                        rootNew2 =
                            updatePropertyByPath rootNew1 (pathOf vOld) ( roleY, asPFloat y )
                    in
                    rootNew2
    in
    List.foldl persistVertexPos eRootNew verticiesOld


graphComparer : Node (Cell a) -> Node (Cell a) -> Bool
graphComparer lRoot rRoot =
    let
        mbLGraph =
            nodesOf (ContentCell GraphCell) lRoot |> List.head

        mbRGraph =
            nodesOf (ContentCell GraphCell) rRoot |> List.head
    in
    case ( mbLGraph, mbRGraph ) of
        ( Nothing, Nothing ) ->
            True

        ( Nothing, Just _ ) ->
            False

        ( Just _, Nothing ) ->
            False

        ( Just lGraph, Just rGraph ) ->
            let
                lVertices =
                    nodesOf (ContentCell VertexCell) lGraph
                        |> List.sortBy (\v -> pathAsIdFromNode v)

                rVerticies =
                    nodesOf (ContentCell VertexCell) rGraph
                        |> List.sortBy (\v -> pathAsIdFromNode v)

                lEdges =
                    nodesOf (ContentCell EdgeCell) lGraph
                        |> List.sortBy (\e -> pathAsIdFromNode e)

                rEdges =
                    nodesOf (ContentCell EdgeCell) rGraph
                        |> List.sortBy (\e -> pathAsIdFromNode e)

                flatIsEqual =
                    flatNodeListComparer (Just []) lVertices rVerticies
                        && flatNodeListComparer (Just []) lEdges rEdges

                lFromTo =
                    fromToPairs lGraph

                rFromTo =
                    fromToPairs rGraph

                numOfRealEdgesIsEqual =
                    List.length lFromTo == List.length rFromTo
            in
            if List.length lVertices /= List.length rVerticies || List.length lEdges /= List.length rEdges then
                False

            else if flatIsEqual == False then
                False

            else if numOfRealEdgesIsEqual == False then
                False

            else
                True


p2dFromCell : Node (Cell a) -> Point2d
p2dFromCell cell =
    P2d.fromCoordinates ( floatOf roleX cell, floatOf roleY cell )


colorGraphPrimary =
    Color.rgb255 17 77 175



-- ROLES


roleDefault =
    roleFromString "default"


roleScope =
    roleFromString "scope"


roleEffects =
    roleFromString "effects"


roleScopeValue =
    roleFromString "scopeValue"


roleName =
    roleFromString "name"


roleInput =
    roleFromString "input"


roleConstant =
    roleFromString "constant"


roleIsHoriz =
    roleFromString "isHoriz"


rolePlaceholder =
    roleFromString "placeholder"


roleIsGrid =
    roleFromString "isGrid"


roleText =
    roleFromString "text"


roleX =
    roleFromString "x"


roleY =
    roleFromString "y"


roleFrom =
    roleFromString "propFrom"


roleTo =
    roleFromString "propTo"


roleIndent =
    roleFromString "indent"


roleMarginTop =
    roleFromString "margin-top"


roleMarginBottom =
    roleFromString "margin-bottom"


roleMarginRight =
    roleFromString "margin-right"


roleMarginLeft =
    roleFromString "margin-left"


roleGrabbed =
    roleFromString "grabbed"
