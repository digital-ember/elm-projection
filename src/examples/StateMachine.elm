module StateMachine exposing (main)

import Structure exposing (..)
import Editor exposing (..)
import Runtime exposing (..)


{-| We need to define a custom domain type.
It contains constructors for each "domain concept" to tag Node_s with via API.
-}
type Domain
    = StateMachine
    | Event
    | State
    | Transition


{-| Program is created by the Runtime.program function.
It requires:
- initial root node (stateMachine)
- editor (top-level function to transform a domain model to a cell model)
-}
main : Program () (Model Domain) (Runtime.Msg Domain)
main =
    program stateMachine editor


{-| Initial root node to start the program with.
Just an root node of variant StateMachine.
-}
stateMachine : Node Domain
stateMachine =
    createRoot StateMachine


{-| Declarative way of building a "state machine editor".
Using the Editor-API, one can stick together cell-based editors.
-}
editor : Node Domain -> Node (Cell Domain)
editor sm =
    createRootCell
        |> with (editorStateMachine sm)


{-| A vertical stack of cells
  * with the editor for the state machine name
  * with the editor for the events
-}
editorStateMachine : Node Domain -> Node (Cell Domain)
editorStateMachine sm =
    vertStackCell
        |> with (editorStateMachineName sm)
        |> with (constantCell "")
        |> with (editorEvents sm)
        |> with (constantCell "")
        |> with (editorStates sm)


{-| A horizontal stack of cells
  * with a constant cell containing "name:"
  * with an input cell containing the "name" property of the state machine
      * input has an onInputEffect node
        Carries the input parameters and the update function itself to be evaluated in the editor update
-}
editorStateMachineName : Node Domain -> Node (Cell Domain)
editorStateMachineName sm =
    horizStackCell
        |> with (constantCell "name:")
        |> with
            (inputCell (textOf "name" sm)
                |> withEffect (onInputEffect ( pathOf sm, "name" ) updateStringProperty)
            )


{-| If our statemachine does not contain any events, we put a placeholder cell to allow the user to add events
Otherwise, we map over all events and create editor cells for them.
The event editors themselves are wrapped by two constant cells ("events" and "end")
-}
editorEvents : Node Domain -> Node (Cell Domain)
editorEvents sm =
    let
        editorEventsResult =
            case getUnderCustom "events" sm of
                Nothing ->
                    [ editorEventPlaceholder sm ]

                Just events ->
                    List.map editorEvent events
    in
        vertStackCell
            |> with
                (constantCell "events")
            |> with
                (vertStackCell
                    |> addIndent
                    |> withRange editorEventsResult
                )
            |> with
                (constantCell "end")


editorEvent : Node Domain -> Node (Cell Domain)
editorEvent event =
    inputCell (textOf "name" event)
        |> withEffect (onEnterEffect (pathOf event) insertNewEventAfter)
        |> withEffect (onDeleteEffect event deleteEvent)
        |> withEffect (onInputEffect ( pathOf event, "name" ) updateStringProperty)


editorEventPlaceholder : Node Domain -> Node (Cell Domain)
editorEventPlaceholder sm =
    placeholderCell "no events"
        |> withEffect (onEnterEffect (pathOf sm) addNewEvent)


editorStates : Node Domain -> Node (Cell Domain)
editorStates sm =
    let
        editorStatesResult =
            case getUnderDefault sm of
                Nothing ->
                    [ editorStatesPlaceholder sm ]

                Just states ->
                    List.map editorState states
    in
        vertStackCell
            |> withRange editorStatesResult


editorStatesPlaceholder : Node Domain -> Node (Cell Domain)
editorStatesPlaceholder sm =
    placeholderCell "no states"
        |> withEffect (onEnterEffect (pathOf sm) addNewState)


editorState : Node Domain -> Node (Cell Domain)
editorState state =
    let
        editorTransitionsResult =
            case getUnderDefault state of
                Nothing ->
                    [ editorTransitionPlaceholder state ]

                Just transitions ->
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
                (placeholderCell "+"
                    |> withEffect (onEnterEffect (pathOf state) insertNewStateAfter)
                )
            |> with (constantCell "")


editorStateHead : Node Domain -> Node (Cell Domain)
editorStateHead state =
    horizStackCell
        |> with (constantCell "state")
        |> with
            (inputCell (textOf "name" state)
                |> withEffect (onInputEffect ( pathOf state, "name" ) updateStringProperty)
                |> withEffect (onEnterEffect (pathOf state) insertNewStateAfter)
            )


editorTransitionPlaceholder : Node Domain -> Node (Cell Domain)
editorTransitionPlaceholder state =
    placeholderCell "no transitions"
        |> withEffect (onEnterEffect (pathOf state) addNewTransition)


editorTransition : Node Domain -> Node (Cell Domain)
editorTransition transition =
    horizStackCell
        |> with
            (inputCell (textOf "eventRef" transition)
                |> withEffect (onInputEffect ( pathOf transition, "eventRef" ) updateStringProperty)
                |> withEffect (onEnterEffect (pathOf transition) insertNewTransitionAfter)
            )
        |> with (constantCell "⇒")
        |> with
            (inputCell (textOf "stateRef" transition)
                |> withEffect (onInputEffect ( pathOf transition, "stateRef" ) updateStringProperty)
                |> withEffect (onEnterEffect (pathOf transition) insertNewTransitionAfter)
            )


{-| This can be considered the "constructor" of a default Event.
We either add it to the list or insert it after the current Event.
This method is passed via a Effect to the editor.
Notice that these replace the need for a "update" method in our domain model (state machine).
We use the Structure API to update our domain model, naturally.
-}
addNewEvent : Node Domain -> Path -> Node Domain
addNewEvent sm _ =
    addToCustom "events"
        (createNode Event
            |> addText "name" ""
        )
        sm


insertNewEventAfter : Node Domain -> Path -> Node Domain
insertNewEventAfter sm pathOfEvent =
    insertAfterUnderCustom "events"
        (createNode Event
            |> addText "name" ""
        )
        pathOfEvent
        sm


addNewState : Node Domain -> Path -> Node Domain
addNewState sm _ =
    addToDefault
        (createNode State
            |> addText "name" ""
        )
        sm


insertNewStateAfter : Node Domain -> Path -> Node Domain
insertNewStateAfter sm pathOfState =
    insertAfterUnderDefault
        (createNode State
            |> addText "name" ""
        )
        pathOfState
        sm


addNewTransition : Node Domain -> Path -> Node Domain
addNewTransition sm pathOfState =
    addChildAtPathToDefault
        (createNode Transition
            |> addText "eventRef" ""
            |> addText "stateRef" ""
        )
        pathOfState
        sm


insertNewTransitionAfter : Node Domain -> Path -> Node Domain
insertNewTransitionAfter sm pathOfTransition =
    insertChildAfterPath 
        (createNode Transition
            |> addText "eventRef" ""
            |> addText "stateRef" ""
        )
        (pathOfTransition |> Debug.log "insert after")
        sm


deleteEvent : Node Domain -> Node Domain -> Node Domain
deleteEvent sm event =
    let
        delete children =
            Just <|
                List.filter (\e -> pathOf e /= pathOf event) children

        mbEventsNew =
            getUnderCustom "events" sm
                |> Maybe.andThen delete
    in
        case mbEventsNew of
            Nothing ->
                sm

            Just eventsNew ->
                replaceUnderCustom "events" eventsNew sm


{-| This pattern allows to update nested records.
Input:
  * Tuple of
      ** domain root (state machine)
      ** path to the node we want to update
      ** key of the property
  * New property value

The actual update can be defered to the Structure module, since it knows the structure (duh!)
-}
updateStringProperty : Node Domain -> ( Path, String ) -> String -> Node Domain
updateStringProperty sm ( path, key ) newValue =
    updatePropertyByPath sm path (stringProperty ( key, newValue ))
