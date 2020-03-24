module Statemachine exposing (main)

import Editor as E
import Runtime as R exposing (Model, projection)
import Structure as S


{-| We need to define a custom domain type.
It contains constructors for each "domain concept" to tag Node\_s with via API.
-}
type Domain
    = Statemachine
    | Event
    | State
    | Transition


roleEventRef =
    S.roleFromString "eventRef"


roleStateRef =
    S.roleFromString "stateRef"


roleEvents =
    S.roleFromString "events"


{-| Program is created by the Runtime.program function.
It requires:

  - initial root node (emptyStatemachine/initStatemachine)
  - editor (top-level function to transform a domain model to a cell model)

-}
main : Program () (Model Domain) (R.Msg Domain)
main =
    projection emptyStatemachine editor


{-| Initial root node to start the program with.
Just an root node of variant Statemachine.
-}
emptyStatemachine : S.Node Domain
emptyStatemachine =
    S.createRoot Statemachine


{-| Declarative way of building a "state machine editor".
Using the Editor-API, one can stick together cell-based editors.
-}
editor : S.Node Domain -> S.Node (E.Cell Domain)
editor sm =
    E.rootCell
        |> E.with (editorStatemachine sm)


{-| A vertical stack of cells

  - with the editor for the state machine name
  - with the editor for the events

-}
editorStatemachine : S.Node Domain -> S.Node (E.Cell Domain)
editorStatemachine sm =
    E.vertSplitCell
        |> E.with
            (E.vertStackCell
                |> E.with (editorStatemachineName sm)
                |> E.with (editorEvents sm)
                |> E.with (editorStates sm)
            )
        |> E.with
            (E.graphCell
                |> E.withRange (editorStatesVerticies sm)
                |> E.withRange (editorTransitionsEdges sm)
            )


{-| A horizontal stack of cells

  - with a constant cell containing "name:"
  - with an input cell containing the "name" property of the state machine
      - input has an onInputEffect node
        Carries the input parameters and the update function itself to be evaluated in the editor update

-}
editorStatemachineName : S.Node Domain -> S.Node (E.Cell Domain)
editorStatemachineName sm =
    E.horizStackCell
        |> E.with (E.constantCell "name:")
        |> E.with (E.inputCell S.roleName sm)
        |> E.addMargin E.Bottom 20


{-| If our statemachine does not contain any events, we put a placeholder cell to allow the user to add events
Otherwise, we map over all events and create editor cells for them.
The event editors themselves are wrapped by two constant cells ("events" and "end")
-}
editorEvents : S.Node Domain -> S.Node (E.Cell Domain)
editorEvents sm =
    let
        editorEventsResult =
            case S.getUnderCustom roleEvents sm of
                [] ->
                    [ editorEventPlaceholder sm ]

                events ->
                    List.map editorEvent events
    in
    E.vertStackCell
        |> E.with (E.constantCell "events")
        |> E.with
            (E.vertStackCell
                |> E.addIndent
                |> E.withRange editorEventsResult
            )
        |> E.with (E.constantCell "end")
        |> E.addMargin E.Bottom 20


editorEvent : S.Node Domain -> S.Node (E.Cell Domain)
editorEvent event =
    E.inputCell S.roleName event
        |> E.withEffect (E.insertionEffect event ctorEvent)
        |> E.withEffect (E.deletionEffect event)


editorEventPlaceholder : S.Node Domain -> S.Node (E.Cell Domain)
editorEventPlaceholder sm =
    E.placeholderCell "no events"
        |> E.withEffect (E.replacementEffect roleEvents sm ctorEvent)


editorStates : S.Node Domain -> S.Node (E.Cell Domain)
editorStates sm =
    let
        editorStatesResult =
            case S.getUnderDefault sm of
                [] ->
                    [ editorStatesPlaceholder sm ]

                states ->
                    List.map editorState states
    in
    E.vertStackCell
        |> E.withRange editorStatesResult


editorStatesPlaceholder : S.Node Domain -> S.Node (E.Cell Domain)
editorStatesPlaceholder sm =
    E.placeholderCell "no states"
        |> E.withEffect (E.replacementEffect S.roleDefault sm ctorState)


editorState : S.Node Domain -> S.Node (E.Cell Domain)
editorState state =
    let
        editorTransitionsResult =
            case S.getUnderDefault state of
                [] ->
                    [ editorTransitionPlaceholder state ]

                transitions ->
                    List.map editorTransition transitions
    in
    E.vertStackCell
        |> E.with (editorStateHead state)
        |> E.with
            (E.vertStackCell
                |> E.addIndent
                |> E.withRange editorTransitionsResult
            )
        |> E.with (E.constantCell "end")
        |> E.with
            (E.buttonCell "+"
                |> E.withEffect (E.insertionEffect state ctorState)
            )
        |> E.addMargin E.Bottom 20


editorStateHead : S.Node Domain -> S.Node (E.Cell Domain)
editorStateHead state =
    E.horizStackCell
        |> E.with (E.constantCell "state")
        |> E.with
            (E.inputCell S.roleName state
                |> E.withEffect (E.insertionEffect state ctorState)
                |> E.withEffect (E.deletionEffect state)
            )


editorTransitionPlaceholder : S.Node Domain -> S.Node (E.Cell Domain)
editorTransitionPlaceholder state =
    E.placeholderCell "no transitions"
        |> E.withEffect (E.replacementEffect S.roleDefault state ctorTransition)


editorTransition : S.Node Domain -> S.Node (E.Cell Domain)
editorTransition transition =
    E.horizStackCell
        |> E.with
            (E.refCell Event roleEventRef transition Nothing
                |> E.withEffect (E.insertionEffect transition ctorTransition)
                |> E.withEffect (E.deletionEffect transition)
            )
        |> E.with (E.constantCell "⇒")
        |> E.with
            (E.refCell State roleStateRef transition Nothing
                |> E.withEffect (E.insertionEffect transition ctorTransition)
                |> E.withEffect (E.deletionEffect transition)
            )


editorStatesVerticies : S.Node Domain -> List (S.Node (E.Cell Domain))
editorStatesVerticies sm =
    List.map editorStateVertex <| S.getUnderDefault sm


editorStateVertex state =
    E.vertexCell S.roleName state


editorTransitionsEdges : S.Node Domain -> List (S.Node (E.Cell Domain))
editorTransitionsEdges sm =
    List.concat <|
        List.map editorTransitionEdge <|
            S.getUnderDefault sm


editorTransitionEdge state =
    let
        edge transition =
            E.edgeCell roleEventRef ( S.textOf S.roleName state, S.textOf roleStateRef transition ) transition
    in
    List.map edge <| S.getUnderDefault state



-- CTORs


ctorEvent : S.Node Domain
ctorEvent =
    S.createNode Event
        |> S.addText S.roleName ""


ctorState : S.Node Domain
ctorState =
    S.createNode State
        |> S.addText S.roleName ""


ctorTransition : S.Node Domain
ctorTransition =
    S.createNode Transition
        |> S.addText roleEventRef ""
        |> S.addText roleStateRef ""



-- DEBUGGING STUFF


initStatemachine : S.Node Domain
initStatemachine =
    S.createRoot Statemachine
        |> S.addRangeToCustom roleEvents
            [ S.createNode Event |> S.addText S.roleName "doorClosed"
            , S.createNode Event |> S.addText S.roleName "drawOpened"
            , S.createNode Event |> S.addText S.roleName "lightOn"
            , S.createNode Event |> S.addText S.roleName "doorOpened"
            , S.createNode Event |> S.addText S.roleName "panelClosed"
            ]
        |> S.addRangeToDefault
            [ S.createNode State
                |> S.addText S.roleName "idle"
                |> S.addToDefault
                    (S.createNode Transition
                        |> S.addText roleEventRef "doorClosed"
                     --|> addText roleStateRef "active"
                    )
            , S.createNode State
                |> S.addText S.roleName "active"
                |> S.addToDefault
                    (S.createNode Transition
                        |> S.addText roleEventRef "drawOpened"
                        |> S.addText roleStateRef "waitingForLight"
                    )
                |> S.addToDefault
                    (S.createNode Transition
                        |> S.addText roleEventRef "lightOn"
                        |> S.addText roleStateRef "waitingForDraw"
                    )
            , S.createNode State
                |> S.addText S.roleName "waitingForLight"
                |> S.addToDefault
                    (S.createNode Transition
                        |> S.addText roleEventRef "lightOn"
                        |> S.addText roleStateRef "unlockedPanel"
                    )
            , S.createNode State
                |> S.addText S.roleName "waitingForDraw"
                |> S.addToDefault
                    (S.createNode Transition
                        |> S.addText roleEventRef "drawOpened"
                        |> S.addText roleStateRef "unlockedPanel"
                    )
            , S.createNode State
                |> S.addText S.roleName "unlockedPanel"
            ]
