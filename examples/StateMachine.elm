module StateMachine exposing (Domain(..), ctorEvent, ctorState, ctorTransition, editor, editorEvent, editorEventPlaceholder, editorEvents, editorState, editorStateHead, editorStateMachine, editorStateMachineName, editorStates, editorStatesPlaceholder, editorTransition, editorTransitionPlaceholder, initStateMachine, main)

import Editor exposing (..)
import Runtime exposing (Model, projection)
import Structure exposing (..)


{-| We need to define a custom domain type.
It contains constructors for each "domain concept" to tag Node\_s with via API.
-}
type Domain
    = StateMachine
    | Event
    | State
    | Transition


roleEventRef =
    roleFromString "eventRef"

roleStateRef =
    roleFromString "stateRef"

roleEvents =
    roleFromString "events"

{-| Program is created by the Runtime.program function.
It requires:

  - initial root node (initStateMachine)
  - editor (top-level function to transform a domain model to a cell model)

-}
main : Program () (Model Domain) (Runtime.Msg Domain)
main =
    projection initStateMachine editor


{-| Initial root node to start the program with.
Just an root node of variant StateMachine.
-}
initStateMachine : Node Domain
initStateMachine =
    createRoot StateMachine
        |> addRangeToCustom roleEvents
            [ createNode Event |> addText roleName "doorClosed"
            , createNode Event |> addText roleName "drawOpened"
            , createNode Event |> addText roleName "lightOn"
            , createNode Event |> addText roleName "doorOpened"
            , createNode Event |> addText roleName "panelClosed"
            ]
        |> addRangeToDefault
            [ createNode State
                |> addText roleName "idle"
                |> addToDefault
                    (createNode Transition
                        |> addText roleEventRef "doorClosed"
                        |> addText roleStateRef "active"
                    )
            , createNode State
                |> addText roleName "active"
                |> addToDefault
                    (createNode Transition
                        |> addText roleEventRef "drawOpened"
                        |> addText roleStateRef "waitingForLight"
                    )
                |> addToDefault
                    (createNode Transition
                        |> addText roleEventRef "lightOn"
                        |> addText roleStateRef "waitingForDraw"
                    )
            , createNode State
                |> addText roleName "waitingForLight"
                |> addToDefault
                    (createNode Transition
                        |> addText roleEventRef "lightOn"
                        |> addText roleStateRef "unlockedPanel"
                    )
            , createNode State
                |> addText roleName "waitingForDraw"
                |> addToDefault
                    (createNode Transition
                        |> addText roleEventRef "drawOpened"
                        |> addText roleStateRef "unlockedPanel"
                    )
            , createNode State
                |> addText roleName "unlockedPanel"

            ]


{-| Declarative way of building a "state machine editor".
Using the Editor-API, one can stick together cell-based editors.
-}
editor : Node Domain -> Node (Cell Domain)
editor sm =
    rootCell
        |> with (editorStateMachine sm)


{-| A vertical stack of cells

  - with the editor for the state machine name
  - with the editor for the events

-}
editorStateMachine : Node Domain -> Node (Cell Domain)
editorStateMachine sm =
    vertSplitCell
        |> with
            (vertStackCell
                |> with (editorStateMachineName sm)
                |> with (editorEvents sm)
                |> with (editorStates sm)
            )
        |> with
            (graphCell
                |> withRange (editorStatesVerticies sm)
                |> withRange (editorTransitionsEdges sm)
            )


{-| A horizontal stack of cells

  - with a constant cell containing "name:"
  - with an input cell containing the "name" property of the state machine
      - input has an onInputEffect node
        Carries the input parameters and the update function itself to be evaluated in the editor update

-}
editorStateMachineName : Node Domain -> Node (Cell Domain)
editorStateMachineName sm =
    horizStackCell
        |> with (constantCell "name:")
        |> with (inputCell roleName sm)
        |> addMargin Bottom 20


{-| If our statemachine does not contain any events, we put a placeholder cell to allow the user to add events
Otherwise, we map over all events and create editor cells for them.
The event editors themselves are wrapped by two constant cells ("events" and "end")
-}
editorEvents : Node Domain -> Node (Cell Domain)
editorEvents sm =
    let
        editorEventsResult =
            case getUnderCustom roleEvents sm of
                [] ->
                    [ editorEventPlaceholder sm ]

                events ->
                    List.map editorEvent events
    in
    vertStackCell
        |> with (constantCell "events")
        |> with
            (vertStackCell
                |> addIndent
                |> withRange editorEventsResult
            )
        |> with (constantCell "end")
        |> addMargin Bottom 20


editorEvent : Node Domain -> Node (Cell Domain)
editorEvent event =
    inputCell roleName event
        |> withEffect (insertionEffect event ctorEvent)
        |> withEffect (deletionEffect event)


editorEventPlaceholder : Node Domain -> Node (Cell Domain)
editorEventPlaceholder sm =
    placeholderCell "no events"
        |> withEffect (replacementEffect roleEvents sm ctorEvent)


editorStates : Node Domain -> Node (Cell Domain)
editorStates sm =
    let
        editorStatesResult =
            case getUnderDefault sm of
                [] ->
                    [ editorStatesPlaceholder sm ]

                states ->
                    List.map editorState states
    in
    vertStackCell
        |> withRange editorStatesResult


editorStatesPlaceholder : Node Domain -> Node (Cell Domain)
editorStatesPlaceholder sm =
    placeholderCell "no states"
        |> withEffect (replacementEffect roleDefault sm ctorState)


editorState : Node Domain -> Node (Cell Domain)
editorState state =
    let
        editorTransitionsResult =
            case getUnderDefault state of
                [] ->
                    [ editorTransitionPlaceholder state ]

                transitions ->
                    List.map editorTransition transitions
    in
    vertStackCell
        |> with (editorStateHead state)
        |> with
            (vertStackCell
                |> addIndent
                |> withRange editorTransitionsResult
            )
        |> with (constantCell "end")
        |> with
            (buttonCell "+"
                |> withEffect (insertionEffect state ctorState)
            )
        |> addMargin Bottom 20


editorStateHead : Node Domain -> Node (Cell Domain)
editorStateHead state =
    horizStackCell
        |> with (constantCell "state")
        |> with
            (inputCell roleName state
                |> withEffect (insertionEffect state ctorState)
                |> withEffect (deletionEffect state)
            )


editorTransitionPlaceholder : Node Domain -> Node (Cell Domain)
editorTransitionPlaceholder state =
    placeholderCell "no transitions"
        |> withEffect (replacementEffect roleDefault state ctorTransition)


editorTransition : Node Domain -> Node (Cell Domain)
editorTransition transition =
    horizStackCell
        |> with
            (refCell Event roleEventRef transition Nothing
                |> withEffect (insertionEffect transition ctorTransition)
                |> withEffect (deletionEffect transition)
            )
        |> with (constantCell "⇒")
        |> with
            (refCell State roleStateRef transition Nothing
                |> withEffect (insertionEffect transition ctorTransition)
                |> withEffect (deletionEffect transition)
            )


editorStatesVerticies : Node Domain -> List (Node (Cell Domain))
editorStatesVerticies sm =
    List.map editorStateVertex <| getUnderDefault sm


editorStateVertex state =
    vertexCell roleName state


editorTransitionsEdges : Node Domain -> List (Node (Cell Domain))
editorTransitionsEdges sm =
    List.concat <|
        List.map editorTransitionEdge <|
            getUnderDefault sm


editorTransitionEdge state =
    let
        edge transition =
            edgeCell roleEventRef ( textOf roleName state, textOf roleStateRef transition ) transition
    in
    List.map edge <| getUnderDefault state



-- CTORs


ctorEvent : Node Domain
ctorEvent =
    createNode Event
        |> addText roleName ""


ctorState : Node Domain
ctorState =
    createNode State
        |> addText roleName ""


ctorTransition : Node Domain
ctorTransition =
    createNode Transition
        |> addText roleEventRef ""
        |> addText roleStateRef ""
