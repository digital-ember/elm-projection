module Editor exposing
    ( Cell
    , EditorModel
    , MarginSide(..)
    , MousePosition
    , Msg(..)
    , addIndent
    , addMargin
    , addSeparator
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
    , persistCollapsedInformation
    , persistGraphInformation
    , placeholderCell
    , refCell
    , replacementEffect
    , resizeCmd
    , roleSeparator
    , rootCell
    , setCollapsible
    , setPropertyEffect
    , styleBold
    , styleBoldItalic
    , styleItalic
    , styleTextColor
    , updateEditor
    , vertGridCell
    , vertSplitCell
    , vertStackCell
    , horizStackCellPH
    , vertStackCellPH
    , vertexCell
    , viewEditor
    , with
    , withEffect
    , withRange
    , withStyle
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
import Svg.Events
import Task as Task
import Triangle2d as T2d exposing (Triangle2d)
import TypedSvg exposing (circle, g, line, rect, svg, text_)
import TypedSvg.Attributes exposing (color, fill, stroke, strokeDasharray)
import TypedSvg.Attributes.InPx exposing (cx, cy, fontSize, height, r, rx, ry, strokeWidth, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (foreignObject)
import TypedSvg.Events as TsvgE
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..))
import Vector2d as V2d exposing (Vector2d)


type Cell isa
    = ContentCell ContentCell
    | EffectCell (EffectCell isa)
    | StyleCell StyleCell


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


type EffectCell isa
    = InsertionEffect (InsertionEffectData isa)
    | DeletionEffect DeletionEffectData
    | InputEffect InputEffectData
    | NavSelectionEffect NavSelectionEffectData
    | CreateScopeEffect (CreateScopeEffectData isa)
    | SetPropertyEffect SetPropertyEffectData


type StyleCell
    = FontStyle FontStyle
    | Color Color


type FontStyle
    = Regular
    | Bold
    | BoldItalic
    | Italic


type alias EditorModel isa =
    { dRoot : Node isa
    , eRoot : Node (Cell isa)
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


type alias InsertionEffectData isa =
    { path : Path
    , nodeToInsert : Node isa
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


type alias CreateScopeEffectData isa =
    { isa : isa
    , pathContextNode : Path
    , scopeProvider : Maybe (List String)
    }


type alias SetPropertyEffectData =
    { pathContextNode : Path
    , role : Role
    , primitive : Primitive
    }


type alias Selection =
    { start : Int
    , end : Int
    , dir : String
    }


type EffectGroup isa
    = InputEffectGroup (List (EffectCell isa))
    | KeyboardEffectGroup (List (EffectCell isa))
    | FocusEffectGroup (List (EffectCell isa))
    | PropertyEffectGroup (List (EffectCell isa))


type Orientation
    = Vert
    | Horiz


type Msg isa
    = NoOp
    | Swallow String
    | NavSelection (EffectCell isa)
    | OnEnter (EffectCell isa) (Node (Cell isa))
    | OnClick (EffectCell isa) (Node (Cell isa))
    | OnBackspace (EffectCell isa) (Node (Cell isa))
    | OnDelete (EffectCell isa) (Node (Cell isa))
    | OnInput (EffectCell isa) String
    | UpdateScope (EffectCell isa)
    | Tick
    | DragStart Path
    | MouseMove Point2d
    | MouseUp Point2d
    | MouseEnter Path
    | MouseLeave Path
    | ResizeVertex Dom.Element (Node (Cell isa))
    | ShowGravity ShowGravityData
    | HintGravity ShowGravityData
    | ManipulateGravity ManipulationData
    | ToggleCollapsed Path


type alias ShowGravityData =
    { doShow : Bool
    , path : Path
    }


type alias ManipulationData =
    { manipulationKind : ManipulationKind
    , path : Path
    }


type ManipulationKind
    = Increase
    | Decrease


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


type alias Entity isa =
    Force.Entity Int { value : Node (Cell isa) }


type alias VertexProperties =
    { posVertex : Point2d
    , posContent : Point2d
    , widthVertex : Float
    , heightVertex : Float
    , angleAreas : List ( Float, Float )
    }



-- CREATION


initEditorModel : Node isa -> Node (Cell isa) -> EditorModel isa
initEditorModel dRoot eRoot =
    { dRoot = dRoot
    , eRoot = eRoot
    , mbSimulation = Nothing
    , drag = Nothing
    , mousePos = P2d.origin
    , runXform = True
    , runSimulation = True
    }


initGravityX =
    400


initGravityY =
    300


initGravityStrength =
    150


rootCell : Node (Cell isa)
rootCell =
    createRoot (ContentCell RootCell)


constantCell : String -> Node (Cell isa)
constantCell constantValue =
    createNode (ContentCell ConstantCell)
        |> addText roleConstant constantValue


refCell : isa -> Role -> Node isa -> Maybe (List String) -> Node (Cell isa)
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


inputCell : Role -> Node isa -> Node (Cell isa)
inputCell role nodeContext =
    createNode (ContentCell InputCell)
        |> addText roleInput (textOf role nodeContext)
        |> withEffect (inputEffect (pathOf nodeContext) role)


horizStackCell : Node (Cell isa)
horizStackCell =
    createNode (ContentCell StackCell)
        |> addBool roleIsHoriz True


vertStackCell : Node (Cell isa)
vertStackCell =
    createNode (ContentCell StackCell)
        |> addBool roleIsHoriz False


vertStackCellPH : String -> Role -> Node isa -> Node isa -> Node (Cell isa)
vertStackCellPH placeholderText role creator parent =
    let
        ph =
            placeholderCell placeholderText
                |> withEffect
                    (replacementEffect role parent creator)

        editorInputCell namedNode =
            inputCell roleName namedNode
                |> withEffect (insertionEffect namedNode creator)
                |> withEffect (deletionEffect namedNode)
    in
    case getUnderCustom role parent of
        [] ->
            ph

        items ->
            vertStackCell
                |> withRange (List.map editorInputCell items)


horizStackCellPH : String -> Role -> Node isa -> Node isa -> Node (Cell isa)
horizStackCellPH placeholderText role creator parent =
    let
        ph =
            placeholderCell placeholderText
                |> withEffect
                    (replacementEffect role parent creator)

        editorInputCell namedNode =
            inputCell roleName namedNode
                |> withEffect (insertionEffect namedNode creator)
                |> withEffect (deletionEffect namedNode)
    in
    case getUnderCustom role parent of
        [] ->
            ph

        items ->
            horizStackCell
                |> withRange (List.map editorInputCell items)


vertSplitCell : Node (Cell isa)
vertSplitCell =
    createNode (ContentCell SplitCell)


horizSplitCell : Node (Cell isa)
horizSplitCell =
    createNode (ContentCell SplitCell)
        |> addBool roleIsHoriz True


vertGridCell : Node (Cell isa)
vertGridCell =
    vertStackCell
        |> addBool roleIsGrid True


placeholderCell : String -> Node (Cell isa)
placeholderCell text =
    createNode (ContentCell PlaceholderCell)
        |> addText rolePlaceholder text


buttonCell : String -> Node (Cell isa)
buttonCell text =
    createNode (ContentCell ButtonCell)
        |> addText roleText text


graphCell : Node (Cell isa)
graphCell =
    createNode (ContentCell GraphCell)
        |> addFloat roleGravityStrength initGravityStrength
        |> addFloat roleGravityX initGravityX
        |> addFloat roleGravityY initGravityY


vertexCell : String -> Node (Cell isa)
vertexCell name =
    createNode (ContentCell VertexCell)
        |> addText roleName name


edgeCell : Role -> ( String, String ) -> Node isa -> Node (Cell isa)
edgeCell role ( from, to ) nodeContext =
    createNode (ContentCell EdgeCell)
        |> addText roleText (textOf role nodeContext)
        |> addText roleFrom from
        |> addText roleTo to


with : Node (Cell isa) -> Node (Cell isa) -> Node (Cell isa)
with node =
    addToDefault node


withRange : List (Node (Cell isa)) -> Node (Cell isa) -> Node (Cell isa)
withRange children =
    addRangeToDefault children


addIndent : Node (Cell isa) -> Node (Cell isa)
addIndent node =
    addBool roleIndent True node


addMargin : MarginSide -> Int -> Node (Cell isa) -> Node (Cell isa)
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


addSeparator : String -> Node (Cell isa) -> Node (Cell isa)
addSeparator separator node =
    addText roleSeparator separator node


setCollapsible : Node (Cell isa) -> Node (Cell isa)
setCollapsible cell =
    case isaOf cell of
        ContentCell StackCell ->
            if boolOf roleIsHoriz cell then
                cell

            else
                addBool roleCollapsible True cell

        _ ->
            cell



-- EFFECTS


withEffect : EffectCell isa -> Node (Cell isa) -> Node (Cell isa)
withEffect effect =
    addToCustom roleEffects <|
        createNode <|
            EffectCell effect


replacementEffect : Role -> Node isa -> Node isa -> EffectCell isa
replacementEffect role nodeContext nodeToInsert =
    InsertionEffect <|
        InsertionEffectData (pathOf nodeContext) nodeToInsert True role


insertionEffect : Node isa -> Node isa -> EffectCell isa
insertionEffect nodeContext nodeToInsert =
    InsertionEffect <|
        InsertionEffectData (pathOf nodeContext) nodeToInsert False roleEmpty


deletionEffect : Node isa -> EffectCell isa
deletionEffect nodeContext =
    DeletionEffect <|
        DeletionEffectData (pathOf nodeContext) emptySelection


inputEffect : Path -> Role -> EffectCell isa
inputEffect path role =
    InputEffect <| InputEffectData path role


createScopeEffect : isa -> Path -> Maybe (List String) -> EffectCell isa
createScopeEffect target path scopeProvider =
    CreateScopeEffect <|
        CreateScopeEffectData target path scopeProvider


setPropertyEffect : Node isa -> Role -> Primitive -> EffectCell isa
setPropertyEffect nodeContext role primitiveToInsert =
    SetPropertyEffect <|
        SetPropertyEffectData (pathOf nodeContext) role primitiveToInsert


navEffects : Path -> List (Cell isa)
navEffects path =
    [ navEffect U path
    , navEffect D path
    , navEffect L path
    , navEffect R path
    ]


navEffect : Dir -> Path -> Cell isa
navEffect dir path =
    EffectCell <|
        NavSelectionEffect
            { dir = dir
            , pathSelectedCell = path
            , selection = emptySelection
            }



-- STYLES


withStyle : StyleCell -> Node (Cell isa) -> Node (Cell isa)
withStyle style =
    addToCustom roleStyles <|
        createNode <|
            StyleCell style


styleBold : StyleCell
styleBold =
    FontStyle Bold


styleBoldItalic : StyleCell
styleBoldItalic =
    FontStyle BoldItalic


styleItalic : StyleCell
styleItalic =
    FontStyle Italic


styleTextColor : Color -> StyleCell
styleTextColor color =
    Color color


emptySelection : Selection
emptySelection =
    { start = -1
    , end = -1
    , dir = ""
    }


grouped : List (Cell isa) -> List (EffectGroup isa)
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

                StyleCell _ ->
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

                        SetPropertyEffect _ ->
                            Dict.update "property" (updateGroup effect) groupDict

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

                "property" ->
                    PropertyEffectGroup v :: effectGroupList

                _ ->
                    effectGroupList
    in
    Dict.foldl toEffectGroupList [] dictGrouped



-- BEHAVIOR


updateEditor : Msg isa -> EditorModel isa -> ( EditorModel isa, Cmd (Msg isa) )
updateEditor msg editorModel =
    case msg of
        ResizeVertex element vertex ->
            let
                eRootNew =
                    updatePropertyByPath
                        (pathOf vertex)
                        ( roleWidth, asPFloat element.element.width )
                        editorModel.eRoot
                        |> updatePropertyByPath
                            (pathOf vertex)
                            ( roleHeight, asPFloat element.element.height )
            in
            ( { editorModel
                | eRoot = eRootNew
                , runXform = False
              }
            , Cmd.none
            )

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

        MouseEnter path ->
            ( { editorModel | eRoot = updatePropertyByPath path ( roleMouseEnter, asPBool True ) editorModel.eRoot, runXform = False }, Cmd.none )

        MouseLeave path ->
            ( { editorModel | eRoot = updatePropertyByPath path ( roleMouseEnter, asPBool False ) editorModel.eRoot, runXform = False }, Cmd.none )

        NoOp ->
            noUpdate editorModel

        Swallow _ ->
            noUpdate editorModel

        OnEnter effect cellContext ->
            updateOnEnterEffect editorModel effect cellContext

        OnClick effect cellContext ->
            updateOnEnterEffect editorModel effect cellContext

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

        ShowGravity showGravityData ->
            updateOnShowGravity editorModel roleShowGravity showGravityData

        HintGravity showGravityData ->
            updateOnShowGravity editorModel roleHintGravity showGravityData

        ManipulateGravity manipulationData ->
            updateOnManipulateGravity editorModel manipulationData

        ToggleCollapsed path ->
            updateOnToggleCollapsed editorModel path


resizeCmd : Node (Cell isa) -> Cmd (Msg isa)
resizeCmd root =
    let
        vertices =
            nodesOf (ContentCell VertexCell) root

        childAndVertex v =
            case getUnderDefault v |> List.head of
                Nothing ->
                    Nothing

                Just child ->
                    Just ( child, v )

        toResizeCmd vertex result =
            case result of
                Ok element ->
                    ResizeVertex element vertex

                Err _ ->
                    NoOp

        cmdFromTuple ( child, v ) =
            Task.attempt (toResizeCmd v) (Dom.getElement (pathAsIdFromNode child))
    in
    Cmd.batch
        (List.map childAndVertex vertices
            |> List.filterMap identity
            |> List.map cmdFromTuple
        )


simulationFromGraph graph =
    let
        vertices =
            nodesOf (ContentCell VertexCell) graph

        gravityX =
            floatOf roleGravityX graph

        gravityY =
            floatOf roleGravityY graph

        forces =
            Force.center gravityX gravityY
                :: [ Force.customLinks 1 <| customEdgeForcesFromGraph graph (floatOf roleGravityStrength graph)
                   , Force.manyBodyStrength -500 <| List.map (\v -> pathAsIdFromNode v) <| vertices
                   ]
    in
    Force.simulation forces


tickGraphSimulations : EditorModel isa -> ( EditorModel isa, Cmd (Msg isa) )
tickGraphSimulations editorModel =
    let
        mbCellGraph =
            nodesOf (ContentCell GraphCell) editorModel.eRoot |> List.head
    in
    case mbCellGraph of
        Nothing ->
            noUpdate editorModel

        Just graph ->
            case editorModel.mbSimulation of
                Nothing ->
                    let
                        simulation =
                            simulationFromGraph graph
                    in
                    ( { editorModel | mbSimulation = Just <| simulation, runXform = False }, resizeCmd editorModel.eRoot )

                Just simulation ->
                    let
                        pathToGraph =
                            pathOf graph

                        vertices =
                            nodesOf (ContentCell VertexCell) graph

                        edges =
                            nodesOf (ContentCell EdgeCell) graph

                        addPosToCell e =
                            e.value
                                |> addFloat roleX e.x
                                |> addFloat roleY e.y

                        entities =
                            List.indexedMap (\i v -> forceEntityFromVertex i v) vertices

                        ( newSimulationState, entitiesNew ) =
                            entities
                                |> Force.tick simulation

                        childrenNew =
                            List.map addPosToCell entitiesNew
                                ++ edges

                        cellGraphNew =
                            replaceUnderFeature roleDefault childrenNew graph

                        eRootNew =
                            replaceChildAtPath cellGraphNew pathToGraph editorModel.eRoot

                        eRootWithDrag =
                            case editorModel.drag of
                                Nothing ->
                                    eRootNew

                                Just drag ->
                                    updateDrag eRootNew drag editorModel.mousePos
                    in
                    ( { editorModel | eRoot = eRootWithDrag, mbSimulation = Just <| newSimulationState, runXform = False }
                    , Cmd.none
                    )


updateDrag : Node (Cell isa) -> Drag -> Point2d -> Node (Cell isa)
updateDrag eRoot drag mousePosCurrent =
    let
        delta =
            V2d.from drag.mousePosStart mousePosCurrent

        ( xNew, yNew ) =
            drag.vertexPosStart |> P2d.translateBy delta |> P2d.coordinates
    in
    updatePropertyByPath drag.path ( roleX, asPFloat xNew ) eRoot
        |> updatePropertyByPath drag.path ( roleY, asPFloat yNew )


updateOnEnterEffect : EditorModel isa -> EffectCell isa -> Node (Cell isa) -> ( EditorModel isa, Cmd (Msg isa) )
updateOnEnterEffect editorModel effect cellContext =
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

        SetPropertyEffect { pathContextNode, role, primitive } ->
            let
                dRootNew =
                    updatePropertyByPath pathContextNode ( role, primitive ) editorModel.dRoot
            in
            ( { editorModel | dRoot = dRootNew, runXform = True }, Cmd.none )

        _ ->
            noUpdate editorModel


updateOnDeleteEffect : EditorModel isa -> EffectCell isa -> Node (Cell isa) -> ( EditorModel isa, Cmd (Msg isa) )
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


updateOnBackspaceEffect : EditorModel isa -> EffectCell isa -> Node (Cell isa) -> ( EditorModel isa, Cmd (Msg isa) )
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


updateOnInputEffect : EditorModel isa -> EffectCell isa -> String -> ( EditorModel isa, Cmd (Msg isa) )
updateOnInputEffect editorModel effect value =
    case effect of
        InputEffect { path, role } ->
            ( { editorModel
                | dRoot = updatePropertyByPath path ( role, asPString value ) editorModel.dRoot |> updatePaths
                , runXform = True
              }
            , Cmd.none
            )

        _ ->
            noUpdate editorModel


updateOnNavEffect : EffectCell isa -> Node (Cell isa) -> Cmd (Msg isa)
updateOnNavEffect effect editorModel =
    case effect of
        NavSelectionEffect navData ->
            updateSelection editorModel navData

        _ ->
            Cmd.none


updateOnCreateScopeEffect : EditorModel isa -> EffectCell isa -> ( EditorModel isa, Cmd (Msg isa) )
updateOnCreateScopeEffect editorModel effect =
    case effect of
        CreateScopeEffect scopeData ->
            ( { editorModel | dRoot = setScopeInformation editorModel.dRoot scopeData, runXform = True }, Cmd.none )

        _ ->
            noUpdate editorModel


updateOnShowGravity : EditorModel isa -> Role -> ShowGravityData -> ( EditorModel isa, Cmd (Msg isa) )
updateOnShowGravity editorModel role showGravityData =
    let
        eRootNew =
            updatePropertyByPath showGravityData.path ( role, asPBool showGravityData.doShow ) editorModel.eRoot
    in
    ( { editorModel | eRoot = eRootNew }, Cmd.none )


updateOnManipulateGravity editorModel manipulationData =
    let
        mbGraphCell =
            nodeAt editorModel.eRoot manipulationData.path
    in
    case mbGraphCell of
        Nothing ->
            noUpdate editorModel

        Just graph ->
            let
                gravityStrengthNew =
                    clamp 50 600 <|
                        case manipulationData.manipulationKind of
                            Increase ->
                                floatOf roleGravityStrength graph + 10

                            Decrease ->
                                floatOf roleGravityStrength graph - 10

                eRootNew =
                    updatePropertyByPath manipulationData.path ( roleGravityStrength, asPFloat gravityStrengthNew ) editorModel.eRoot

                ( runSimulationNew, mbSimulationNew ) =
                    case editorModel.mbSimulation of
                        Nothing ->
                            ( True, Nothing )

                        Just sim ->
                            ( False, Just (simulationFromGraph graph) )
            in
            ( { editorModel
                | eRoot = eRootNew
                , runSimulation = runSimulationNew
                , mbSimulation = mbSimulationNew
              }
            , Cmd.none
            )


updateOnToggleCollapsed : EditorModel a -> Path -> ( EditorModel a, Cmd (Msg a) )
updateOnToggleCollapsed editorModel path =
    let
        mbNodeToToggle =
            nodeAt editorModel.eRoot path
    in
    case mbNodeToToggle of
        Nothing ->
            noUpdate editorModel

        Just nodeToToggle ->
            let
                toggleTo =
                    boolOf roleCollapsed nodeToToggle == False

                eRootNew =
                    updatePropertyByPath path ( roleCollapsed, asPBool toggleTo ) editorModel.eRoot
            in
            ( { editorModel
                | eRoot = eRootNew
              }
            , Cmd.none
            )


noUpdate : EditorModel a -> ( EditorModel a, Cmd (Msg a) )
noUpdate editorModel =
    ( { editorModel | runXform = False }, Cmd.none )


setScopeInformation : Node isa -> CreateScopeEffectData isa -> Node isa
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


tryDelete : EditorModel isa -> DeletionEffectData -> (Node isa -> Path -> Maybe (Node isa)) -> Int -> Bool -> ( EditorModel isa, Cmd (Msg isa) )
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


updateSelectionOnEnter : Node (Cell isa) -> Cmd (Msg isa)
updateSelectionOnEnter cellContext =
    Task.perform
        NavSelection
    <|
        Task.succeed <|
            NavSelectionEffect <|
                NavSelectionEffectData D (pathOf cellContext) <|
                    Selection 0 0 ""


updateSelection : Node (Cell isa) -> NavSelectionEffectData -> Cmd (Msg isa)
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


updateSelectionByOrientation : Node (Cell isa) -> NavSelectionEffectData -> Orientation -> Cmd (Msg isa)
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


findPrevInputCell : Node (Cell isa) -> Node (Cell isa) -> Node (Cell isa)
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


findPrevInputCellRec : Node (Cell isa) -> Node (Cell isa) -> Maybe (Node (Cell isa))
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


findNextInputCell : Node (Cell isa) -> Node (Cell isa) -> Node (Cell isa)
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


findNextInputCellRec : Node (Cell isa) -> Node (Cell isa) -> Maybe (Node (Cell isa))
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


findFirstInputCellRec : Node (Cell isa) -> List (Node (Cell isa)) -> (Node (Cell isa) -> Node (Cell isa) -> Maybe (Node (Cell isa))) -> Maybe (Node (Cell isa))
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


viewEditor : Node (Cell isa) -> Html (Msg isa)
viewEditor root =
    div [ HtmlA.style "font-family" "Consolas" ] <|
        case isaOf root of
            ContentCell _ ->
                viewCell root

            EffectCell _ ->
                []

            StyleCell _ ->
                []


viewCell : Node (Cell isa) -> List (Html (Msg isa))
viewCell cell =
    case isaOf cell of
        ContentCell StackCell ->
            case getUnderDefault cell of
                [] ->
                    [ viewEmpty ]

                children ->
                    case tryTextOf roleSeparator cell of
                        Nothing ->
                            List.foldl viewContent [] children

                        Just separator ->
                            List.foldl viewContent [] children
                                |> List.intersperse
                                    (div
                                        [ HtmlA.style "margin-left" "-5px"
                                        , HtmlA.style "margin-right" "5px"
                                        ]
                                        [ text separator ]
                                    )

        ContentCell _ ->
            case getUnderDefault cell of
                [] ->
                    [ viewEmpty ]

                children ->
                    List.foldl viewContent [] children

        EffectCell _ ->
            []

        StyleCell _ ->
            []


viewEmpty =
    text ""


viewContent : Node (Cell isa) -> List (Html (Msg isa)) -> List (Html (Msg isa))
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
                            viewEmpty

                        EdgeCell ->
                            viewEmpty
            in
            htmlNew :: List.reverse html |> List.reverse

        EffectCell _ ->
            []

        StyleCell _ ->
            []


viewSplitCell : Node (Cell isa) -> Html (Msg isa)
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
            viewEmpty

        StyleCell _ ->
            viewEmpty


viewVertSplit : Node (Cell isa) -> Html (Msg isa)
viewVertSplit cell =
    case isaOf cell of
        ContentCell _ ->
            let
                splits =
                    case getUnderDefault cell of
                        [] ->
                            [ [ text "Completely empty split cell" ]
                            , [ viewEmpty ]
                            ]

                        first :: [] ->
                            [ viewContent first []
                            , [ text "Empty right side" ]
                            ]

                        moreThanTwo ->
                            List.map (\split -> viewContent split []) moreThanTwo

                numOfSplits =
                    List.length splits

                totalWidth =
                    (100 // numOfSplits)
                        * (numOfSplits - 1)
                        + 100
                        |> String.fromInt

                widthFactor i =
                    if i == (numOfSplits - 1) then
                        1

                    else
                        numOfSplits

                divFromSplit i split =
                    div
                        (styleSplitVert (widthFactor i))
                        split

                divsFromSplit =
                    List.indexedMap
                        divFromSplit
                        splits
            in
            div
                [ HtmlA.style "display" "flex"
                , HtmlA.style "width" (totalWidth ++ "%")
                ]
                divsFromSplit

        EffectCell _ ->
            viewEmpty

        StyleCell _ ->
            viewEmpty


viewHorizSplit : Node (Cell isa) -> Html (Msg isa)
viewHorizSplit cell =
    case isaOf cell of
        ContentCell _ ->
            let
                ( top, bottom ) =
                    case getUnderDefault cell of
                        [] ->
                            ( [ text "Completely empty split cell" ], [ viewEmpty ] )

                        first :: [] ->
                            ( viewCell first
                            , [ text "Empty bottom side" ]
                            )

                        first :: (second :: _) ->
                            ( viewContent first []
                            , viewContent second []
                            )
            in
            div styleSplitHoriz
                [ div
                    styleSplitTop
                    top
                , div
                    styleSplitBottom
                    bottom
                ]

        EffectCell _ ->
            viewEmpty

        StyleCell _ ->
            viewEmpty


viewStackCell : Node (Cell isa) -> Html (Msg isa)
viewStackCell cell =
    case isaOf cell of
        ContentCell _ ->
            if boolOf roleIsHoriz cell then
                viewHorizStackCell cell

            else
                viewVertStackCell cell

        EffectCell _ ->
            viewEmpty

        StyleCell _ ->
            viewEmpty


viewVertStackCell : Node (Cell isa) -> Html (Msg isa)
viewVertStackCell cell =
    case isaOf cell of
        ContentCell _ ->
            if boolOf roleCollapsible cell then
                viewCollapsibleVertStackCell cell

            else
                div
                    ([ HtmlA.id (pathAsIdFromNode cell)
                     , HtmlA.style "display" "table"
                     ]
                        ++ marginsAndPaddings cell
                    )
                <|
                    viewCell cell

        EffectCell _ ->
            viewEmpty

        StyleCell _ ->
            viewEmpty


viewCollapsibleVertStackCell cell =
    case isaOf cell of
        ContentCell _ ->
            let
                collapseButton =
                    div [ HtmlA.style "display" "table-cell" ]
                        [ svg
                            [ HtmlA.style "width" "16"
                            , HtmlA.style "height" "12"
                            ]
                            [ g [ Svg.Events.onClick <| ToggleCollapsed (pathOf cell) ]
                                ([ circle
                                    [ r 4
                                    , cx 6
                                    , cy 6
                                    , strokeWidth 1
                                    , stroke <| Paint <| Color.darkBlue
                                    , fill <| Paint <| Color.white
                                    ]
                                    []
                                 , line
                                    [ x1 3
                                    , y1 6
                                    , x2 9
                                    , y2 6
                                    , stroke <| Paint <| Color.darkBlue
                                    ]
                                    []
                                 ]
                                    ++ (if boolOf roleCollapsed cell then
                                            [ line
                                                [ x1 6
                                                , y1 3
                                                , x2 6
                                                , y2 9
                                                , stroke <| Paint <| Color.darkBlue
                                                ]
                                                []
                                            ]

                                        else
                                            []
                                       )
                                )
                            ]
                        ]
            in
            div
                ([ HtmlA.style "display" "flex" ] ++ marginsAndPaddings cell)
                [ collapseButton
                , div
                    [ HtmlA.id (pathAsIdFromNode cell)
                    , HtmlA.style "display" "table"
                    ]
                  <|
                    if boolOf roleCollapsed cell then
                        [ text "[...]" ]

                    else
                        viewCell cell
                ]

        EffectCell _ ->
            viewEmpty

        StyleCell _ ->
            viewEmpty


viewHorizStackCell : Node (Cell isa) -> Html (Msg isa)
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
            viewEmpty

        StyleCell _ ->
            viewEmpty


viewConstantCell : Node (Cell isa) -> Html (Msg isa)
viewConstantCell cell =
    case isaOf cell of
        ContentCell _ ->
            div
                (divCellAttributes cell)
                [ label
                    ((HtmlA.id (pathAsIdFromNode cell)
                        :: styleAttributesFromCell cell
                     )
                        ++ marginsAndPaddings cell
                    )
                    [ text (textOf roleConstant cell) ]
                ]

        EffectCell _ ->
            viewEmpty

        StyleCell _ ->
            viewEmpty


viewInputCell : Node (Cell isa) -> Html (Msg isa)
viewInputCell cell =
    case isaOf cell of
        ContentCell _ ->
            let
                inputValue =
                    textOf roleInput cell

                inputSize =
                    if inputValue == "" then
                        String.length "<no value>" - 1

                    else
                        String.length inputValue - 1
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
                     , HtmlA.size
                        (if inputSize < 1 then
                            1

                         else
                            inputSize
                        )
                     , HtmlA.id (pathAsIdFromNode cell)
                     ]
                        ++ marginsAndPaddings cell
                        ++ inputCellAttributesFromEffects cell
                    )
                    []
                ]

        EffectCell _ ->
            viewEmpty

        StyleCell _ ->
            viewEmpty


viewPlaceholderCell : Node (Cell isa) -> Html (Msg isa)
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
            viewEmpty

        StyleCell _ ->
            viewEmpty


viewButtonCell : Node (Cell isa) -> Html (Msg isa)
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
            viewEmpty

        StyleCell _ ->
            viewEmpty


viewRefCell : Node (Cell isa) -> Html (Msg isa)
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
            viewEmpty

        StyleCell _ ->
            viewEmpty


viewGraphCell : Node (Cell isa) -> Html (Msg isa)
viewGraphCell cellGraph =
    let
        hintGravity =
            boolOf roleHintGravity cellGraph

        gravity =
            if hintGravity then
                [ g [] <|
                    [ viewGravity cellGraph ]
                , g []
                    [ text_
                        [ x 10
                        , y 20
                        , fill <| Paint colorGravityPrimary
                        ]
                        [ text <| "Gravity Control Mode" ]
                    , text_
                        [ x 10
                        , y 40
                        , fill <| Paint colorGravityPrimary
                        , fontSize 14
                        ]
                        [ text <| "gravity strength: " ++ (String.fromFloat <| floatOf roleGravityStrength cellGraph) ]
                    ]
                ]

            else
                []
    in
    svg
        [ HtmlA.style "width" "100%"
        , HtmlA.style "height" "800px"
        , HtmlA.style "background-color" "AliceBlue"
        , gravityOnMouseMove HintGravity (pathOf cellGraph)
        ]
        ([ g [] <|
            viewEdgeCells cellGraph
         , g [] <|
            List.map viewVertexCell <|
                nodesOf (ContentCell VertexCell) cellGraph
         ]
            ++ gravity
        )


gravityOnMouseMove : (ShowGravityData -> Msg isa) -> Path -> Attribute (Msg isa)
gravityOnMouseMove msg path =
    Svg.Events.on "mousemove" <|
        JsonD.map
            (\shift ->
                msg { doShow = shift, path = path }
            )
        <|
            JsonD.field "shiftKey" JsonD.bool


viewGravity : Node (Cell isa) -> TypedSvg.Core.Svg (Msg isa)
viewGravity cellGraph =
    let
        onMouseOut =
            Svg.Events.onMouseOut <| ShowGravity { doShow = False, path = pathOf cellGraph }

        path =
            pathOf cellGraph

        radius =
            abs (floatOf roleGravityStrength cellGraph) / 2
    in
    if boolOf roleShowGravity cellGraph then
        circle
            [ r radius
            , cx <| floatOf roleGravityX cellGraph
            , cy <| floatOf roleGravityY cellGraph
            , stroke PaintNone
            , strokeWidth 2
            , fill <| Paint <| colorGravityPrimary
            , Svg.Events.on "mousewheel" (decodeMouseEvent path)
            , onMouseOut
            , gravityOnMouseMove ShowGravity path
            ]
            []

    else
        circle
            [ r radius
            , cx <| floatOf roleGravityX cellGraph
            , cy <| floatOf roleGravityY cellGraph
            , stroke <| Paint colorGravityPrimary
            , strokeWidth 2
            , strokeDasharray "4"
            , fill <| Paint <| Color.rgba 0 0 0 0
            , onMouseOut
            , gravityOnMouseMove ShowGravity path
            ]
            []


decodeMouseEvent : Path -> JsonD.Decoder (Msg a)
decodeMouseEvent path =
    JsonD.map
        (\dY ->
            ManipulateGravity
                { manipulationKind =
                    if dY < 0 then
                        Decrease

                    else
                        Increase
                , path = path
                }
        )
        (JsonD.field "deltaY" JsonD.float)


viewEdgeCells : Node (Cell isa) -> List (Html (Msg isa))
viewEdgeCells cellGraph =
    List.map viewEdgeCell (fromToPairs cellGraph)


viewEdgeCell : ( Node (Cell isa), Node (Cell isa) ) -> Html (Msg isa)
viewEdgeCell fromTo =
    let
        ( edgeStart, edgeEnd ) =
            vertexAnchorsForEdge fromTo

        edgeLine =
            LS2d.from edgeStart edgeEnd
    in
    edgeWithArrowHead edgeLine


vertexAnchorsForEdge : ( Node (Cell isa), Node (Cell isa) ) -> ( Point2d, Point2d )
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

        sectorFromAngle isa =
            isa
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


vertexProperties : Node (Cell isa) -> VertexProperties
vertexProperties vertex =
    let
        widthVertex =
            floatOf roleWidth vertex + 20

        heightVertex =
            floatOf roleHeight vertex + 20

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
    in
    { posVertex = posVertex
    , posContent = posContent
    , widthVertex = widthVertex
    , heightVertex = heightVertex
    , angleAreas = angleAreas
    }


viewVertexCell : Node (Cell isa) -> Html (Msg isa)
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


vertexContent : Node (Cell isa) -> VertexProperties -> Html (Msg isa)
vertexContent cell { posContent, widthVertex, heightVertex } =
    foreignObject
        [ x <| P2d.xCoordinate posContent
        , y <| P2d.yCoordinate posContent
        , width <| widthVertex - 20
        , height <| heightVertex - 20
        ]
    <|
        viewCell cell


vertexDragHandle : Node (Cell isa) -> VertexProperties -> Html (Msg isa)
vertexDragHandle cell { posVertex } =
    let
        attributes =
            [ r 5
            , cx <| P2d.xCoordinate posVertex
            , cy <| P2d.yCoordinate posVertex
            , stroke <| Paint <| colorGraphPrimary
            , strokeWidth 2
            , fill <| Paint <| colorGraphBackground
            ]
                ++ (if boolOf roleGrabbed cell then
                        [ r 8
                        , fill <| Paint <| colorGraphPrimary
                        ]

                    else if boolOf roleMouseEnter cell then
                        [ r 8
                        , TsvgE.onMouseDown (DragStart (pathOf cell))
                        , TsvgE.onMouseLeave (MouseLeave (pathOf cell))
                        ]

                    else
                        [ TsvgE.onMouseEnter (MouseEnter (pathOf cell)) ]
                   )
    in
    circle attributes []


edgeWithArrowHead : LS2d.LineSegment2d -> Html (Msg isa)
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


optionFromScope : Node (Cell isa) -> Html (Msg isa)
optionFromScope scopeElement =
    let
        scopeValue =
            textOf roleConstant scopeElement
    in
    option
        [ HtmlA.value scopeValue ]
        []


divRowAttributes : Node (Cell isa) -> List (Attribute (Msg isa))
divRowAttributes cell =
    if boolOf roleIsGrid cell then
        [ HtmlA.style "display" "table-row" ]

    else
        []


divCellAttributes : Node (Cell isa) -> List (Attribute (Msg isa))
divCellAttributes cell =
    if boolOf roleIsGrid cell then
        [ HtmlA.style "display" "table-cell" ]

    else
        []


styleAttributesFromCell : Node (Cell isa) -> List (Html.Attribute (Msg isa))
styleAttributesFromCell cell =
    let
        styles =
            isasUnderCustom roleStyles cell
    in
    List.map attributeFromStyle styles
        |> List.concat


attributeFromStyle : Cell isa -> List (Html.Attribute (Msg isa))
attributeFromStyle styleCell =
    case styleCell of
        StyleCell style ->
            case style of
                FontStyle fontStyle ->
                    case fontStyle of
                        Regular ->
                            [ HtmlA.style "font-weight" "regular" ]

                        Bold ->
                            [ HtmlA.style "font-weight" "bold" ]

                        Italic ->
                            [ HtmlA.style "font-style" "italic" ]

                        BoldItalic ->
                            [ HtmlA.style "font-weight" "bold", HtmlA.style "font-style" "italic" ]

                Color color ->
                    [ HtmlA.style "color" <| Color.toCssString color ]

        _ ->
            []


marginsAndPaddings : Node (Cell isa) -> List (Attribute (Msg isa))
marginsAndPaddings cell =
    [ margins cell, paddings cell ]


margins : Node (Cell isa) -> Attribute (Msg isa)
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


paddings : Node (Cell isa) -> Attribute (Msg isa)
paddings _ =
    HtmlA.style "padding" <| "0px 0px 0px 0px"


inputCellAttributesFromEffects : Node (Cell isa) -> List (Attribute (Msg isa))
inputCellAttributesFromEffects cell =
    let
        effectGroups =
            grouped <|
                isasUnderCustom roleEffects cell
                    ++ navEffects (pathOf cell)
    in
    List.map (attributeFromEffectGroup cell) effectGroups
        |> List.filterMap identity


attributeFromEffectGroup : Node (Cell isa) -> EffectGroup isa -> Maybe (Attribute (Msg isa))
attributeFromEffectGroup cell effectGroup =
    case effectGroup of
        InputEffectGroup effects ->
            case effects of
                effect :: [] ->
                    Just (HtmlE.onInput (OnInput effect))

                _ ->
                    Nothing

        KeyboardEffectGroup effects ->
            Just (effectAttributeFromKey (inputEffectMap cell effects))

        FocusEffectGroup effects ->
            case effects of
                effect :: [] ->
                    Just (HtmlE.onFocus (UpdateScope effect))

                _ ->
                    Nothing

        PropertyEffectGroup effects ->
            case effects of
                effect :: [] ->
                    -- todo: this is not correct, the whole effect handling is not optimal atm
                    Just (HtmlE.onClick (OnClick effect cell))

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


inputEffectMap : Node (Cell isa) -> List (EffectCell isa) -> Dict.Dict String (Msg isa)
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

                SetPropertyEffect _ ->
                    dict
        )
        Dict.empty
        effects


effectAttributeFromKey : Dict.Dict String (Msg isa) -> Attribute (Msg isa)
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


orientationOf : Node (Cell isa) -> Node (Cell isa) -> Maybe Orientation
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


griddify : Node (Cell isa) -> Node (Cell isa)
griddify =
    griddifyI False


griddifyI : Bool -> Node (Cell isa) -> Node (Cell isa)
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


forceEntityFromVertex : Int -> Node (Cell isa) -> Force.Entity String { value : Node (Cell isa) }
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


dictNameToVertex : Node (Cell isa) -> Dict.Dict String (Node (Cell isa))
dictNameToVertex cellGraph =
    nodesOf (ContentCell VertexCell) cellGraph
        |> List.foldl
            (\v d ->
                Dict.insert
                    (textOf roleName v)
                    v
                    d
            )
            Dict.empty


edgeForcesFromGraph : Node (Cell isa) -> List ( String, String )
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


customEdgeForcesFromGraph : Node (Cell isa) -> Float -> List { source : String, target : String, distance : Float, strength : Maybe Float }
customEdgeForcesFromGraph cellGraph distance =
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
                            , distance = distance
                            , strength = Nothing
                            }
    in
    List.map forceLookup edges
        |> List.filterMap identity


fromToPairs : Node (Cell isa) -> List ( Node (Cell isa), Node (Cell isa) )
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


persistCollapsedInformation : Node (Cell isa) -> Node (Cell isa) -> Node (Cell isa)
persistCollapsedInformation eRootOld eRootNew =
    let
        mbCollapsed =
            tryBoolOf roleCollapsed eRootOld
    in
    case mbCollapsed of
        Nothing ->
            List.foldl persistCollapsedInformation eRootNew (getUnderDefault eRootOld)

        Just collapsed ->
            let
                eRootNew2 =
                    updatePropertyByPath (pathOf eRootOld) ( roleCollapsed, asPBool collapsed ) eRootNew
            in
            List.foldl persistCollapsedInformation eRootNew2 (getUnderDefault eRootOld)


persistGraphInformation : Node (Cell isa) -> Node (Cell isa) -> Node (Cell isa)
persistGraphInformation eRootOld eRootNew =
    persistVertexPositions eRootOld eRootNew
        |> persistGravityInformation eRootOld


persistGravityInformation : Node (Cell isa) -> Node (Cell isa) -> Node (Cell isa)
persistGravityInformation eRootOld eRootNew =
    let
        graphsOld =
            nodesOf (ContentCell GraphCell) eRootOld

        persistGravity gOld rootNew =
            let
                mbGravityStrength =
                    tryFloatOf roleGravityStrength gOld

                mbGravityX =
                    tryFloatOf roleGravityX gOld

                mbGravityY =
                    tryFloatOf roleGravityY gOld

                persistGravityStrength =
                    case mbGravityStrength of
                        Nothing ->
                            rootNew

                        Just strength ->
                            updatePropertyByPath (pathOf gOld) ( roleGravityStrength, asPFloat strength ) rootNew

                persistGravityPos root =
                    case ( mbGravityX, mbGravityY ) of
                        ( Nothing, _ ) ->
                            root

                        ( _, Nothing ) ->
                            root

                        ( Just x, Just y ) ->
                            updatePropertyByPath (pathOf gOld) ( roleGravityX, asPFloat x ) root
                                |> updatePropertyByPath (pathOf gOld) ( roleGravityY, asPFloat y )
            in
            persistGravityStrength
                |> persistGravityPos
    in
    List.foldl persistGravity eRootNew graphsOld


persistVertexPositions : Node (Cell isa) -> Node (Cell isa) -> Node (Cell isa)
persistVertexPositions eRootOld eRootNew =
    let
        verticesOld =
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
                    updatePropertyByPath (pathOf vOld) ( roleX, asPFloat x ) rootNew
                        |> updatePropertyByPath (pathOf vOld) ( roleY, asPFloat y )
    in
    List.foldl persistVertexPos eRootNew verticesOld


graphComparer : Node (Cell isa) -> Node (Cell isa) -> Bool
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

                rVertices =
                    nodesOf (ContentCell VertexCell) rGraph
                        |> List.sortBy (\v -> pathAsIdFromNode v)

                lEdges =
                    nodesOf (ContentCell EdgeCell) lGraph
                        |> List.sortBy (\e -> pathAsIdFromNode e)

                rEdges =
                    nodesOf (ContentCell EdgeCell) rGraph
                        |> List.sortBy (\e -> pathAsIdFromNode e)

                flatIsEqual =
                    flatNodeListComparer (Just []) lVertices rVertices
                        && flatNodeListComparer (Just []) lEdges rEdges

                lFromTo =
                    fromToPairs lGraph

                rFromTo =
                    fromToPairs rGraph

                numOfRealEdgesIsEqual =
                    List.length lFromTo == List.length rFromTo
            in
            if List.length lVertices /= List.length rVertices || List.length lEdges /= List.length rEdges then
                False

            else if flatIsEqual == False then
                False

            else if numOfRealEdgesIsEqual == False then
                False

            else
                True


p2dFromCell : Node (Cell isa) -> Point2d
p2dFromCell cell =
    P2d.fromCoordinates ( floatOf roleX cell, floatOf roleY cell )


colorGraphPrimary =
    Color.rgb255 17 77 175


colorGraphBackground =
    Color.rgb255 240 248 255


colorGravityPrimary =
    Color.rgba 1.0 0 0 0.5



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


roleSeparator =
    roleFromString "separator"


roleCollapsible =
    roleFromString "collapsible"


roleCollapsed =
    roleFromString "collapsed"


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


roleWidth =
    roleFromString "width"


roleHeight =
    roleFromString "height"


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


roleMouseEnter =
    roleFromString "mouseEnter"


roleShowGravity =
    roleFromString "showGravity"


roleGravityStrength =
    roleFromString "gravityStrength"


roleGravityX =
    roleFromString "gravityX"


roleGravityY =
    roleFromString "gravityY"


roleHintGravity =
    roleFromString "hintGravity"


roleStyles =
    roleFromString "styles"


styleSplit =
    [ --HtmlA.style "z-index" "1"
      --, HtmlA.style "top" "0"
      --, HtmlA.style "overflow-x" "hidden"
      HtmlA.style "padding" "1%"
    ]


styleSplitHoriz =
    styleSplit
        ++ [ HtmlA.style "width" "100%"
           , HtmlA.style "height" "100%"
           , HtmlA.style "display" "table"
           , HtmlA.style "border-collapse" "collapse"
           ]


styleSplitTop =
    [ HtmlA.style "display" "table-row"
    , HtmlA.style "height" "48%"
    , HtmlA.style "padding" "1%"
    ]


styleSplitBottom =
    [ HtmlA.style "display" "table-row"
    , HtmlA.style "height" "48%"
    , HtmlA.style "border-top" "2px solid"
    , HtmlA.style "padding" "1%"
    ]


styleSplitVert num =
    let
        widthAsString =
            100 // num |> String.fromInt
    in
    [ HtmlA.style "padding" "10px"
    , HtmlA.style "box-sizing" "border-box"
    , HtmlA.style "width" (widthAsString ++ "%")
    , HtmlA.style "overflow" "auto"
    , HtmlA.style "height" "97.5vh"
    , HtmlA.style "border" "1px solid"
    ]


styleSplitRight =
    styleSplit
        ++ [ HtmlA.style "right" "0"
           , HtmlA.style "position" "absolute"
           , HtmlA.style "height" "100%"
           , HtmlA.style "width" "58%"
           ]
