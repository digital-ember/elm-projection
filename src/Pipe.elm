module Pipe exposing (CellMsg(..), Cell, pipeProgram, createRootCell, constantCell, propertyCell, vertStackCell, onCellClick)

{-| Manages the transformation from a application domain to the 
Cell domain and the communication of message events backwards.
-}

import Browser
import Html exposing (div, span, text, input, Html)
import Html.Events as HtmlE
import Html.Attributes as HtmlA
import Browser.Dom as Dom
import Task
import Json.Decode as JsonD


{-| The main type of this module. 
Holds a (application) root and a transformation function from the application domain to the Cell domain. 

-}
type alias Pipe root appMsg =
    { root : root
    , xform : root -> Cell appMsg
    }


{-| Top level messages. 
NoOp is send when the selection is updated via a Task.
PipeMsg is just a top level message to wrap messages to the Cell domain.
-}
type Msg appMsg
    = PipeMsg (CellMsg appMsg)
    | NoOp


{-| Main function for clients to create Program.
You pass your model, the transformation to the Cell domain, and an update function to react to application-relevant messages.
-}
pipeProgram : root -> (root -> Cell appMsg) -> (appMsg -> root -> root) -> Program () (Pipe root appMsg) (Msg appMsg)
pipeProgram root xform appUpdate =
    let
        init () =
            ( { root = root, xform = xform }, Cmd.none )

        update msg pipe =
            pipeUpdate appUpdate msg pipe

    in
        Browser.element
            { init = init
            , update = update
            , view = view
            , subscriptions = \_ -> Sub.none
            }


{-| Only needs to delegate to the "first domain from right" at the moment
(the one that produces the Html, i.e. the Cell domain's update function).

Might handle pipe-relevant messages if there are any, or go away to simplify the code, in the future. 
-}
pipeUpdate : (appMsg -> root -> root) -> Msg appMsg -> Pipe root appMsg -> ( Pipe root appMsg, Cmd (Msg appMsg) )
pipeUpdate appUpdate msg pipe =
    case msg of
        NoOp ->
            ( pipe, Cmd.none )

        PipeMsg cmdMsg ->
            cellUpdate appUpdate cmdMsg pipe


{-| Our forward transformation is handled here. 
The application root is transformed into a "Cell tree", which, in turn, can be rendered. 
-}
view : Pipe root appMsg -> Html (Msg appMsg)
view pipe =
    let
        cellRoot =
            pipe.xform pipe.root
    in
        renderCell cellRoot



-- CELL DOMAIN

{-| Might need it's own module. It is the intended target domain for applications.
Instead of producing Html, applications produce a Cell tree which handles the details on how to be transformed to Html.
The Cell domain lacks functionality, of course, it is just a minimal prototype.

As per my "vision", in the future, there could be many transformations between the "application domain" and the "Cell domain".
To keep it simple, the Pipe expects a direct transformation from application to Cell domain. 
----------------

Cell type wraps around different KCell variant (KCell -> Kind of Cell).
It is basically a adapter to Html.

I implemented a very basic and lacking way of putting "id_s" on Cells to be able to set the focus on certain user interactions.
This is just not good yet. 
Conceptually, what we would need is a strong ID for every "node" in a "tree". 
In the cell domain this means each cell needs to be easily identifyable. But this would also have to be true for the application domain's model.
Then, when a message comes in to the update function, it should hold some sort of "path" backwards through the transformation chain 
to easily identify which model elements in each domain are going to be affected be this message.
-}
type Cell appMsg
    = Cell
        String
        (List (CellAttribute (CellMsg appMsg)))
        (KCell appMsg)

{-| RootCell is just a "helper" to have a fixed start to produce IDs from.
ConstantCell is non-editable, PropertyCell is editable
StackCell is a collection of other cells, but the StackOrientation is currently not really taken into account at all.
-}
type KCell appMsg
    = RootCell (List (Cell appMsg))
    | ConstantCell String
    | PropertyCell String
    | StackCell StackOrientation (List (Cell appMsg))

{-|not really used yet-}
type StackOrientation
    = Horiz
    | Vert

{-|A limited set of messages that are interesting to the Cell domain.
Input is meant to update a PropertyCell's underlying application model String so that users would not have to handle such low level events
by themselves all the time.
In order for this to work, there must be the contract that the passed String actually is part of the underlying application model.

I'm not sure if it is even possible to achieve this in Elm, due to it's immutability. 
This is somehting I would definitly like to talk about it a bit more: 
"what are ways in Elm to easily access and update model data that is depply nested inside a Model?"

NavSelection is just a very basic approach to make a POC of handling messages that update the selection in the Cell model.
AppMsg is a wrapper around application-relevant messages. 
-}
type CellMsg appMsg
    = Input (Cell appMsg) String
    | NavSelection (Cell appMsg) Dir
    | AppMsg appMsg

{-| Direction for selection navigation -}
type Dir
    = Up
    | Down

{-| Adapter for Html.Attribute -}
type CellAttribute msg
    = CellAttribute (Html.Attribute msg)


{-|This is required to be used in order to create the IDs correctly
IDs are just Strings encoding a cells position in the tree. 
"0" is root, "00" is the first child under root, "01" the second child, and so forth.
-}
createRootCell : List (Cell appMsg) -> Cell appMsg
createRootCell children =
    Cell "0" [] (RootCell (List.indexedMap (addIds "0") children))

{-|recursively attach IDs to everything below the root cell-}
addIds : String -> Int -> Cell appMsg -> Cell appMsg
addIds parentId localIndex cell =
    let
        (Cell _ attributes kcell) =
            cell

        parentIdNew =
            parentId ++ String.fromInt localIndex
    in
        case kcell of
            RootCell _ ->
                cell

            StackCell orientation children ->
                Cell parentIdNew attributes <|
                    StackCell orientation (List.indexedMap (addIds parentIdNew) children)

            ConstantCell txt ->
                Cell parentIdNew attributes (ConstantCell txt)

            PropertyCell txt ->
                Cell parentIdNew attributes (PropertyCell txt)


withId : String -> Int -> List (Cell appMsg) -> List (Cell appMsg)
withId parentId nextId children =
    List.indexedMap (\i (Cell _ attributes kcell) -> Cell (parentId ++ String.fromInt (nextId + i)) attributes kcell) children


constantCell : List (CellAttribute (CellMsg appMsg)) -> String -> Cell appMsg
constantCell attributes txt =
    Cell "" attributes (ConstantCell txt)


propertyCell : List (CellAttribute (CellMsg appMsg)) -> String -> Cell appMsg
propertyCell attributes txt =
    Cell "" attributes (PropertyCell txt)


vertStackCell : List (CellAttribute (CellMsg appMsg)) -> List (Cell appMsg) -> Cell appMsg
vertStackCell attributes cells =
    Cell "" attributes (StackCell Vert cells)


cellUpdate : (appMsg -> root -> root) -> CellMsg appMsg -> Pipe root appMsg -> ( Pipe root appMsg, Cmd (Msg appMsg) )
cellUpdate appUpdate msg pipe =
    case msg of
        Input cell text ->
            ( pipe, Cmd.none )

        -- this is where the backmapping to the actual app model must happen somehow: via a dict maintained by the pipe? 
        -- just by passing on a appMsg?
        NavSelection (Cell id _ _) dir ->
            let
                idNew =
                    id
                        |> if dir == Up then
                            getPrev
                           else
                            getNext

                focus =
                    Dom.focus idNew
            in
                ( pipe, Task.attempt (\_ -> NoOp) focus )

        AppMsg appMsg ->
            ( { pipe | root = appUpdate appMsg pipe.root }, Cmd.none )

{-| Very poor and incomplete implementations of getting previous and next IDs based on a current one.-}
getPrev : String -> String
getPrev id =
    let
        mbLast =
            String.reverse id
                |> String.uncons
    in
        case mbLast of
            Nothing ->
                id

            Just ( last, tail ) ->
                if Char.toCode last < 1 then
                    id
                    -- cannot handle yet
                else
                    String.cons (Char.fromCode (Char.toCode last - 1)) tail
                        |> String.reverse



getNext : String -> String
getNext id =
    let
        mbLast =
            String.reverse id
                |> String.uncons
    in
        case mbLast of
            Nothing ->
                id

            Just ( last, tail ) ->
                String.cons (Char.fromCode (Char.toCode last + 1)) tail |> String.reverse



renderCell : Cell appMsg -> Html (Msg appMsg)
renderCell cell =
    let
        (Cell id attributes kcell) =
            cell
    in
        case kcell of
            RootCell cells ->
                renderStackCell id [] Vert cells

            ConstantCell txt ->
                renderConstantCell id attributes cell txt

            PropertyCell txt ->
                renderPropertyCell id attributes cell txt

            StackCell orientation cells ->
                renderStackCell id attributes orientation cells


renderConstantCell : String -> List (CellAttribute (CellMsg appMsg)) -> Cell appMsg -> String -> Html (Msg appMsg)
renderConstantCell id attributes cell txt =
    span (HtmlA.id id :: liftAttr attributes) [ text txt ]


renderPropertyCell : String -> List (CellAttribute (CellMsg appMsg)) -> Cell appMsg -> String -> Html (Msg appMsg)
renderPropertyCell id attributes cell txt =
    div []
        [ input
            ([ HtmlA.id id
             , HtmlA.map (\cellMsg -> PipeMsg cellMsg) (produceKeyboardMsg cell)
             , HtmlA.placeholder "<no value>"
             , HtmlA.value txt
             , HtmlA.map (\cellMsg -> PipeMsg cellMsg) (HtmlE.onInput (Input cell))
             ]
                ++ liftAttr attributes
            )
            []
        ]


renderStackCell : String -> List (CellAttribute (CellMsg appMsg)) -> StackOrientation -> List (Cell appMsg) -> Html (Msg appMsg)
renderStackCell id attributes orientation cells =
    div
        (HtmlA.style "margin" "0" :: liftAttr attributes)
        [ div
            [ HtmlA.style "margin" "3px 20px"
            , HtmlA.id id
            ]
          <|
            List.map renderCell cells
        ]


liftAttr : List (CellAttribute (CellMsg appMsg)) -> List (Html.Attribute (Msg appMsg))
liftAttr attributes =
    List.map (\(CellAttribute htmlAttr) -> htmlAttr) attributes
        |> List.map (HtmlA.map (\cellMsg -> PipeMsg cellMsg))



-- CELL ATTRIBUTES


onCellClick : msg -> CellAttribute msg
onCellClick msg =
    CellAttribute (HtmlE.onClick msg)


produceKeyboardMsg : Cell appMsg -> Html.Attribute (CellMsg appMsg)
produceKeyboardMsg cell =
    let
        canHandle c =
            case c of
                38 ->
                    -- UP
                    JsonD.succeed (NavSelection cell Up)

                40 ->
                    -- DOWN
                    JsonD.succeed (NavSelection cell Down)

                _ ->
                    JsonD.fail ("incorrect code: " ++ String.fromInt c)
    in
        HtmlE.on "keydown" (JsonD.andThen canHandle HtmlE.keyCode)
