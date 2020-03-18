module Editor exposing
    ( Cell
    , MarginSide(..)
    , Msg
    , addIndent
    , addMargin
    , buttonCell
    , constantCell
    , createRootCell
    , deletionEffect
    , griddify
    , horizStackCell
    , inputCell
    , inputEffect
    , insertionEffect
    , placeholderCell
    , refCell
    , replacementEffect
    , updateEditor
    , vertGridCell
    , vertStackCell
    , viewEditor
    , with
    , withEffect
    , withRange
    )

import Browser.Dom as Dom
import Dict as Dict
import Html exposing (..)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD
import Structure exposing (..)
import Task as Task


type Cell a
    = ContentCell ContentCell
    | EffectCell (EffectCell a)


type ContentCell
    = RootCell
    | StackCell
    | ConstantCell
    | InputCell
    | PlaceholderCell
    | ButtonCell
    | RefCell


type EffectCell a
    = InsertionEffect (InsertionEffectData a)
    | DeletionEffect DeletionEffectData
    | InputEffect InputEffectData
    | NavSelectionEffect NavSelectionEffectData
    | CreateScopeEffect (CreateScopeEffectData a)


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
    = -- will be needed for selection update
      NoOp
    | Swallow String
    | NavSelection (EffectCell a)
    | OnEnter (EffectCell a) (Node (Cell a))
    | OnClick (EffectCell a) (Node (Cell a))
    | OnBackspace (EffectCell a) (Node (Cell a))
    | OnDelete (EffectCell a) (Node (Cell a))
    | OnInput (EffectCell a) String
    | UpdateScope (EffectCell a)


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


createRootCell : Node (Cell a)
createRootCell =
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
        |> addToCustomRange featScope (createRefScope nodeContext)
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


with : Node (Cell a) -> Node (Cell a) -> Node (Cell a)
with node =
    addToDefault node


withRange : List (Node (Cell a)) -> Node (Cell a) -> Node (Cell a)
withRange children =
    addToDefaultRange children


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


updateEditor : Msg a -> Node (Cell a) -> Node a -> ( Node a, Cmd (Msg a) )
updateEditor msg editorModel domainModel =
    case msg of
        NoOp ->
            ( domainModel, Cmd.none )

        Swallow _ ->
            ( domainModel, Cmd.none )

        OnEnter effect cellContext ->
            updateOnInsertionEffect domainModel effect cellContext

        OnClick effect cellContext ->
            updateOnInsertionEffect domainModel effect cellContext

        OnDelete effect cellContext ->
            updateOnDeleteEffect domainModel effect cellContext

        OnBackspace effect cellContext ->
            updateOnBackspaceEffect domainModel effect cellContext

        OnInput effect value ->
            updateOnInputEffect domainModel effect value

        NavSelection effect ->
            ( domainModel, updateOnNavEffect effect editorModel )

        UpdateScope effect ->
            updateOnCreateScopeEffect domainModel effect


updateOnInsertionEffect : Node a -> EffectCell a -> Node (Cell a) -> ( Node a, Cmd (Msg a) )
updateOnInsertionEffect domainModel effect cellContext =
    case effect of
        InsertionEffect { path, nodeToInsert, isReplace, feature } ->
            if isReplace then
                ( addChildAtPath feature nodeToInsert path domainModel |> updatePaths, Cmd.none )

            else
                ( insertChildAfterPath nodeToInsert path domainModel |> updatePaths, updateSelectionOnEnter cellContext )

        _ ->
            ( domainModel, Cmd.none )


updateOnDeleteEffect : Node a -> EffectCell a -> Node (Cell a) -> ( Node a, Cmd (Msg a) )
updateOnDeleteEffect domainModel effect cellContext =
    case effect of
        DeletionEffect ({ selection } as effectData) ->
            let
                textLength =
                    textOf propInput cellContext |> String.length

                isAtDeletePos =
                    selection.end == textLength
            in
            ( tryDelete
                domainModel
                effectData
                nextSibling
                textLength
                isAtDeletePos
            , Cmd.none
            )

        _ ->
            ( domainModel, Cmd.none )


updateOnBackspaceEffect : Node a -> EffectCell a -> Node (Cell a) -> ( Node a, Cmd (Msg a) )
updateOnBackspaceEffect domainModel effect cellContext =
    case effect of
        DeletionEffect ({ selection } as effectData) ->
            let
                textLength =
                    textOf propInput cellContext |> String.length

                isAtDeletePos =
                    selection.start == 0
            in
            ( tryDelete
                domainModel
                effectData
                previousSibling
                textLength
                isAtDeletePos
            , Cmd.none
            )

        _ ->
            ( domainModel, Cmd.none )


updateOnInputEffect : Node a -> EffectCell a -> String -> ( Node a, Cmd (Msg a) )
updateOnInputEffect domainModel effect value =
    case effect of
        InputEffect { path, key } ->
            ( updatePropertyByPath domainModel path ( key, value ) |> updatePaths, Cmd.none )

        _ ->
            ( domainModel, Cmd.none )


updateOnNavEffect : EffectCell a -> Node (Cell a) -> Cmd (Msg a)
updateOnNavEffect effect editorModel =
    case effect of
        NavSelectionEffect navData ->
            updateSelection editorModel navData

        _ ->
            Cmd.none


updateOnCreateScopeEffect : Node a -> EffectCell a -> ( Node a, Cmd (Msg a) )
updateOnCreateScopeEffect domainModel effect =
    case effect of
        CreateScopeEffect scopeData ->
            ( setScopeInformation domainModel scopeData, Cmd.none )

        _ ->
            ( domainModel, Cmd.none )


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


tryDelete : Node a -> DeletionEffectData -> (Node a -> Path -> Maybe (Node a)) -> Int -> Bool -> Node a
tryDelete domainModel { path } navFun textLength isAtDeletePos =
    if textLength == 0 then
        deleteNodeUnder path domainModel |> updatePaths

    else if isAtDeletePos then
        let
            mbNext =
                navFun domainModel path
        in
        case mbNext of
            Nothing ->
                domainModel

            Just next ->
                deleteNodeUnder (pathOf next) domainModel |> updatePaths

    else
        domainModel


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
            in
            htmlNew :: List.reverse html |> List.reverse

        EffectCell _ ->
            []


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
