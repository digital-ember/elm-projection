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
        , buttonCell
        , addIndent
        , withEffect
        , insertionEffect
        , replacementEffect
        , onDeleteEffect
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
        { effectInput : Node a
        , effectHandler : Node a -> Node a -> Node a
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


createRootCell : Node (Cell a)
createRootCell =
    createRoot (ContentCell RootCell)


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



-- EFFECTS


withEffect : Effect a -> Node (Cell a) -> Node (Cell a)
withEffect effect =
    addToCustom "effects" <|
        createNode <|
            EffectCell effect


replacementEffect : String -> Path -> Node a -> Effect a
replacementEffect feature path nodeToInsert =
    InsertionEffect
        { path = path
        , nodeToInsert = nodeToInsert
        , isReplace = True
        , feature = feature
        }


insertionEffect : Path -> Node a -> Effect a
insertionEffect path nodeToInsert =
    InsertionEffect
        { path = path
        , nodeToInsert = nodeToInsert
        , isReplace = False
        , feature = ""
        }


onDeleteEffect : Node a -> (Node a -> Node a -> Node a) -> Effect a
onDeleteEffect effectInput effectHandler =
    OnDeleteEffect
        { effectInput = effectInput
        , effectHandler = effectHandler
        , selection = { start = -1, end = -1, dir = "" }
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
            , selection =
                { start = -1
                , end = -1
                , dir = ""
                }
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
                    , updateSelectionOnEnter editorModel domainModel cellContext
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
                    , updateSelectionOnEnter editorModel domainModel cellContext
                    )

                _ ->
                    ( domainModel, Cmd.none )

        OnDelete effect cellContext ->
            case effect of
                OnDeleteEffect effectData ->
                    ( tryDeleteRight domainModel effectData (textOf "input" cellContext |> String.length), Cmd.none )

                _ ->
                    ( domainModel, Cmd.none )

        OnBackspace effect cellContext ->
            case effect of
                OnDeleteEffect effectData ->
                    ( tryDeleteLeft domainModel effectData (textOf "input" cellContext |> String.length), Cmd.none )

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
                    ( domainModel, updateSelection editorModel domainModel navData )

                _ ->
                    ( domainModel, Cmd.none )


tryDeleteLeft : Node a -> { effectInput : Node a, effectHandler : Node a -> Node a -> Node a, selection : Selection } -> Int -> Node a
tryDeleteLeft domainModel { effectInput, effectHandler, selection } textLength =
    if textLength == 0 then
        effectHandler domainModel effectInput |> updatePaths
    else if selection.start == 0 then
        let
            mbPrev =
                previousSibling domainModel (pathOf effectInput)
        in
            case mbPrev of
                Nothing ->
                    domainModel

                Just prev ->
                    effectHandler domainModel prev |> updatePaths
    else
        domainModel


tryDeleteRight : Node a -> { effectInput : Node a, effectHandler : Node a -> Node a -> Node a, selection : Selection } -> Int -> Node a
tryDeleteRight domainModel { effectInput, effectHandler, selection } textLength =
    if textLength == 0 then
        effectHandler domainModel effectInput |> updatePaths
    else if selection.start == textLength then
        let
            mbNext =
                nextSibling domainModel (pathOf effectInput)
        in
            case mbNext of
                Nothing ->
                    domainModel

                Just next ->
                    effectHandler domainModel next |> updatePaths
    else
        domainModel


updateSelectionOnEnter : Node (Cell a) -> Node a -> Node (Cell a) -> Cmd (Msg a)
updateSelectionOnEnter editorModel domainModel cellContext =
    let
        navData =
            { dir = D
            , cellSelected = cellContext
            , selection = { start = 0, end = 0, dir = "" }
            }
    in
        updateSelectionByOrientation editorModel domainModel navData Vert


updateSelection : Node (Cell a) -> Node a -> { dir : Dir, cellSelected : Node (Cell a), selection : Selection } -> Cmd (Msg a)
updateSelection editorModel domainModel navData =
    let
        mbOrientation =
            orientationOf editorModel navData.cellSelected
    in
        case mbOrientation of
            Nothing ->
                Cmd.none

            Just orientation ->
                updateSelectionByOrientation editorModel domainModel navData orientation


updateSelectionByOrientation : Node (Cell a) -> Node a -> { dir : Dir, cellSelected : Node (Cell a), selection : Selection } -> Orientation -> Cmd (Msg a)
updateSelectionByOrientation editorModel domainModel { dir, cellSelected, selection } orientation =
    let
        mover op =
            move editorModel domainModel cellSelected op
    in
        case ( dir, orientation ) of
            ( U, Vert ) ->
                mover (-)

            ( D, Vert ) ->
                mover (+)

            ( L, Horiz ) ->
                if selection.start == 0 then
                    mover (-)
                else
                    Cmd.none

            ( R, Horiz ) ->
                if selection.start >= (textOf "input" cellSelected |> String.length) then
                    mover (+)
                else
                    Cmd.none

            _ ->
                Cmd.none


move : Node (Cell a) -> Node a -> Node (Cell a) -> (Int -> Int -> Int) -> Cmd (Msg a)
move editorModel domainModel cellSelected op =
    let
        nextPathAsId parentPath feature index =
            --pathAsIdFromNode (findNextInputCell editorModel cellSelected)
            pathAsId parentPath ++ "-" ++ feature ++ String.fromInt (op index 1)

        d =
            pathAsIdFromNode (findNextInputCell editorModel cellSelected) |> Debug.log "nid"

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
            let
                mbChildren =
                    getUnderDefault next
            in
                case mbChildren of
                    Just children ->
                        let
                            mbFirst =
                                findFirstInputCellRec root children
                        in
                            case mbFirst of
                                Nothing ->
                                    Just <| findNextInputCell root next

                                Just first ->
                                    Just first

                    Nothing ->
                        Just <| findNextInputCell root next

        _ ->
            Just <| findNextInputCell root next


findFirstInputCellRec : Node (Cell a) -> List (Node (Cell a)) -> Maybe (Node (Cell a))
findFirstInputCellRec root candidates =
    case candidates of
        [] ->
            Nothing

        head :: tail ->
            let
                mbFirst =
                    findNextInputCellRec root head
            in
                case mbFirst of
                    Nothing ->
                        findFirstInputCellRec root tail

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
                    boolOf "isHoriz" cell |> Debug.log "isHoriz"
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
                    , HtmlA.style "font-weight" "bold"
                    , HtmlA.style "color" "darkblue"
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
                []
                [ input
                    ([ HtmlA.style "border-width" "0px"
                     , HtmlA.style "margin" "0px 3px 0px 0px"
                     , HtmlA.style "font-family" "Consolas"
                     , HtmlA.style "font-size" "16px"
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
                     , HtmlA.style "font-family" "Consolas"
                     , HtmlA.style "font-size" "16px"
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
                button onClick [ text (textOf "text" cell) ]

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
