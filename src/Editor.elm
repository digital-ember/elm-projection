module Editor
    exposing
        ( Cell
        , Msg
        , MarginSide(..)
        , createRootCell
        , with
        , withRange
        , constantCell
        , inputCell
        , horizStackCell
        , vertStackCell
        , placeholderCell
        , buttonCell
        , addIndent
        , addMargin
        , withEffect
        , insertionEffect
        , replacementEffect
        , deletionEffect
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
import Result as Result


type Cell a
    = ContentCell ContentCell
    | EffectCell (Effect a)


type ContentCell
    = RootCell
    | StackCell
    | ConstantCell
    | InputCell
    | PlaceholderCell
    | ButtonCell


type Effect a
    = InsertionEffect
        { path : Path
        , nodeToInsert : Node a
        , isReplace : Bool
        , feature : String
        }
    | OnDeleteEffect
        { path : Path 
        , selection : Selection
        }
    | OnInputEffect
        { path : Path
        , key : String
        }
    | NavSelectionEffect
        { dir : Dir
        , cellSelected : Node (Cell a)
        , selection : Selection
        }


type alias Selection =
    { start : Int
    , end : Int
    , dir : String
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
    | OnEnter (Effect a) (Node (Cell a))
    | OnClick (Effect a) (Node (Cell a))
    | OnBackspace (Effect a) (Node (Cell a))
    | OnDelete (Effect a) (Node (Cell a))
    | OnInput (Effect a) String


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
        |> addText "constant" text


inputCell : String -> Node a -> Node (Cell a)
inputCell text nodeContext =
    createNode (ContentCell InputCell)
        |> addText "input" (textOf text nodeContext)
        |> withEffect (onInputEffect (pathOf nodeContext) text)


horizStackCell : Node (Cell a)
horizStackCell =
    createNode (ContentCell StackCell)
        |> addBool "isHoriz" True


vertStackCell : Node (Cell a)
vertStackCell =
    createNode (ContentCell StackCell)
        |> addBool "isHoriz" False


placeholderCell : String -> Node (Cell a)
placeholderCell text =
    createNode (ContentCell PlaceholderCell)
        |> addText "placeholder" text


buttonCell : String -> Node (Cell a)
buttonCell text =
    createNode (ContentCell ButtonCell)
        |> addText "text" text


with : Node (Cell a) -> Node (Cell a) -> Node (Cell a)
with node =
    addToDefault node


withRange : List (Node (Cell a)) -> Node (Cell a) -> Node (Cell a)
withRange children =
    addToDefaultRange children


addIndent : Node (Cell a) -> Node (Cell a)
addIndent node =
    addBool "indent" True node


addMargin : MarginSide -> Int -> Node (Cell a) -> Node (Cell a)
addMargin side space node =
    let
        key = 
            case side of
                Top -> 
                    "margin-top" 
                
                Right -> 
                    "margin-right"

                Bottom -> 
                    "margin-bottom"

                Left -> 
                    "margin-left"
    in
        addInt key space node


-- EFFECTS


withEffect : Effect a -> Node (Cell a) -> Node (Cell a)
withEffect effect =
    addToCustom "effects" <|
        createNode <|
            EffectCell effect


replacementEffect : String -> Node a -> Node a -> Effect a
replacementEffect feature nodeContext nodeToInsert =
    InsertionEffect
        { path = pathOf nodeContext
        , nodeToInsert = nodeToInsert
        , isReplace = True
        , feature = feature
        }


insertionEffect : Node a -> Node a -> Effect a
insertionEffect nodeContext nodeToInsert =
    InsertionEffect
        { path = pathOf nodeContext
        , nodeToInsert = nodeToInsert 
        , isReplace = False
        , feature = ""
        }


deletionEffect : Node a -> Effect a
deletionEffect nodeContext =
    OnDeleteEffect
        { path = pathOf nodeContext 
        , selection = emptySelection
        }


onInputEffect : Path -> String -> Effect a
onInputEffect path key =
    OnInputEffect
        { path = path
        , key = key
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
    EffectCell <|
        NavSelectionEffect
            { dir = dir
            , cellSelected = cell
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
                        OnInputEffect _ ->
                            Dict.update "input" (updateGroup effect) groupDict

                        InsertionEffect _ ->
                            Dict.update "keyboard" (updateGroup effect) groupDict

                        OnDeleteEffect _ ->
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

        OnEnter effect cellContext ->
            case effect of
                InsertionEffect { path, nodeToInsert, isReplace, feature } ->
                    ( if isReplace then
                        if feature == "" || feature == "default" then
                            addChildAtPathToDefault nodeToInsert path domainModel |> updatePaths
                        else
                            addChildAtPathToCustom feature nodeToInsert path domainModel |> updatePaths
                      else
                        insertChildAfterPath nodeToInsert path domainModel |> updatePaths
                    , updateSelectionOnEnter cellContext
                    )

                _ ->
                    ( domainModel, Cmd.none )

        OnClick effect cellContext ->
            case effect of
                InsertionEffect { path, nodeToInsert, isReplace, feature } ->
                    ( if isReplace then
                        if feature == "" || feature == "default" then
                            addChildAtPathToDefault nodeToInsert path domainModel |> updatePaths
                        else
                            addChildAtPathToCustom feature nodeToInsert path domainModel |> updatePaths
                      else
                        insertChildAfterPath nodeToInsert path domainModel |> updatePaths
                    , updateSelectionOnEnter cellContext
                    )

                _ ->
                    ( domainModel, Cmd.none )

        OnDelete effect cellContext ->
            case effect of
                OnDeleteEffect ({selection} as effectData) ->
                    let
                        textLength = textOf "input" cellContext |> String.length
                        isAtDeletePos = selection.end == textLength
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

        OnBackspace effect cellContext ->
            case effect of
                OnDeleteEffect ({selection} as effectData) ->
                    let
                        textLength = textOf "input" cellContext |> String.length
                        isAtDeletePos = selection.start == 0 
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

        OnInput effect value ->
            case effect of
                OnInputEffect { path, key } ->
                    ( updatePropertyByPath domainModel path ( key, value ) |> updatePaths, Cmd.none )

                _ ->
                    ( domainModel, Cmd.none )

        NavSelection effect ->
            case effect of
                NavSelectionEffect navData ->
                    ( domainModel, updateSelection editorModel navData )

                _ ->
                    ( domainModel, Cmd.none )


tryDelete :  Node a -> { path : Path, selection : Selection } -> (Node a -> Path -> Maybe (Node a)) -> Int -> Bool -> Node a
tryDelete domainModel { path, selection } navFun textLength isAtDeletePos =
    if textLength == 0 then
        deleteNode path domainModel |> updatePaths
    else if isAtDeletePos then
        let
            mbNext =
                navFun domainModel path
        in
            case mbNext of
                Nothing ->
                    domainModel

                Just next ->
                    deleteNode (pathOf next) domainModel |> updatePaths
    else
        domainModel


updateSelectionOnEnter : Node (Cell a) -> Cmd (Msg a)
updateSelectionOnEnter cellContext =
    Task.perform
        NavSelection
        (Task.succeed
            (NavSelectionEffect { dir = D, cellSelected = cellContext, selection = { dir = "", start = 0, end = 0 } })
        )


updateSelection : Node (Cell a) -> { dir : Dir, cellSelected : Node (Cell a), selection : Selection } -> Cmd (Msg a)
updateSelection editorModel navData =
    let
        mbOrientation =
            orientationOf editorModel navData.cellSelected
    in
        case mbOrientation of
            Nothing ->
                Cmd.none

            Just orientation ->
                updateSelectionByOrientation editorModel navData orientation


updateSelectionByOrientation : Node (Cell a) -> { dir : Dir, cellSelected : Node (Cell a), selection : Selection } -> Orientation -> Cmd (Msg a)
updateSelectionByOrientation editorModel { dir, cellSelected, selection } orientation =
    let
        moverTask f =
            Task.attempt
                (\result ->
                    case result of
                        Result.Err _ ->
                            NavSelection (NavSelectionEffect { dir = D, cellSelected = cellSelected, selection = selection })

                        _ ->
                            NoOp
                )
                (Dom.focus <| pathAsIdFromNode (f editorModel cellSelected))
    in
        case ( dir, orientation ) of
            ( U, Vert ) ->
                moverTask findPrevInputCell

            ( D, Vert ) ->
                moverTask findNextInputCell

            ( L, Horiz ) ->
                if selection.start == 0 then
                    moverTask findPrevInputCell
                else
                    Cmd.none

            ( R, Horiz ) ->
                if selection.start >= (textOf "input" cellSelected |> String.length) then
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
                    boolOf "isHoriz" cell
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
                (HtmlA.id (pathAsIdFromNode cell)
                :: marginsAndPaddings cell)
            <|
                viewCell cell

        EffectCell _ ->
            text ""


viewHorizStackCell : Node (Cell a) -> Html (Msg a)
viewHorizStackCell cell =
    case isaOf cell of
        ContentCell _ ->
            div
                
                (
                [ HtmlA.style "display" "flex"
                , HtmlA.id (pathAsIdFromNode cell)
                ]
                ++ 
                marginsAndPaddings cell
                )
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
                    (
                    [ HtmlA.id (pathAsIdFromNode cell)
                    , HtmlA.style "font-weight" "bold"
                    , HtmlA.style "color" "darkblue"
                    ]
                    ++ 
                    marginsAndPaddings cell
                    )
                    [ text (textOf "constant" cell) ]
                ]

        EffectCell _ ->
            text ""


viewInputCell : Node (Cell a) -> Html (Msg a)
viewInputCell cell =
    case isaOf cell of
        ContentCell _ ->
            let
                inputValue = textOf "input" cell
                inputSize = if inputValue == "" then String.length "<no value>" else String.length inputValue
            in
            
                div
                    []
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
            let
                placeholderValue = textOf "placeholder" cell

                inputValue = 
                    "<" ++ 
                        if placeholderValue == "" then
                            "..."
                        else 
                            placeholderValue
                        ++ 
                    ">"
                inputSize = String.length inputValue
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
                            ++ createInputCellAttributes cell
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
                    isasUnderCustom "effects" cell
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
                button (marginsAndPaddings cell ++ onClick) [ text (textOf "text" cell) ]

        EffectCell _ ->
            text ""


marginsAndPaddings : Node (Cell a) -> List (Attribute (Msg a))
marginsAndPaddings cell = 
    [margins cell, paddings cell]

margins : Node (Cell a) -> Attribute (Msg a)
margins cell = 
    let
        indentMarginLeft = if boolOf "indent" cell then 20 else 0

        top = (intOf "margin-top" cell |> String.fromInt) ++ "px "
        right = ((intOf "margin-right" cell + 5) |> String.fromInt) ++ "px "
        bottom = (intOf "margin-bottom" cell |> String.fromInt) ++ "px "
        left = ((intOf "margin-left" cell + indentMarginLeft) |> String.fromInt) ++ "px"

    in
        HtmlA.style "margin" <| top ++ right ++ bottom ++ left


paddings : Node (Cell a) -> Attribute (Msg a)
paddings cell =
    HtmlA.style "padding" <| "0px 0px 0px 0px"


createInputCellAttributes : Node (Cell a) -> List (Attribute (Msg a))
createInputCellAttributes cell =
    let
        effectGroups =
            grouped <|
                isasUnderCustom "effects" cell
                    ++ navEffects cell
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


inputEffectMap : Node (Cell a) -> List (Effect a) -> Dict.Dict String (Msg a)
inputEffectMap cell effects =
    List.foldl
        (\effect dict ->
            case effect of
                InsertionEffect _ ->
                    Dict.insert "Enter" (OnEnter effect cell) dict

                OnDeleteEffect _ ->
                    Dict.insert "Delete" (OnDelete effect cell) <|
                        Dict.insert "Backspace" (OnBackspace effect cell) dict

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
                        JsonD.fail ("incorrect code: " ++ k |> Debug.log "incorrect")

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
                                    OnDeleteEffect effectData ->
                                        JsonD.map
                                            (\sel ->
                                                OnDelete (OnDeleteEffect { effectData | selection = sel }) cellContext
                                            )
                                            (JsonD.field "target" decodeSelection)

                                    _ ->
                                        JsonD.succeed msg

                            OnBackspace effect cellContext ->
                                case effect of
                                    OnDeleteEffect effectData ->
                                        JsonD.map
                                            (\sel ->
                                                OnBackspace (OnDeleteEffect { effectData | selection = sel }) cellContext
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
                        boolOf "isHoriz" cell
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
