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
        |> with (editorEvents sm)


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
                |> withEffect (onInputEffect ( sm, pathOf sm, "name" ) updateStringProperty)
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
                    editorEventPlaceholder sm

                Just events ->
                    List.map (editorEvent sm) events
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


editorEvent : Node Domain -> Node Domain -> Node (Cell Domain)
editorEvent sm event =
    inputCell (textOf "name" event)
        |> withEffect (onEnterEffect ( sm, Just event ) addNewEvent)
        |> withEffect (onInputEffect ( sm, pathOf event, "name" ) updateStringProperty)


editorEventPlaceholder : Node Domain -> List (Node (Cell Domain))
editorEventPlaceholder sm =
    [ placeholderCell "no events"
        |> withEffect (onEnterEffect ( sm, Nothing ) addNewEvent)
    ]


{-| This can be considered the "constructor" of a default Event.
We either add it to the list or insert it after the current Event.
This method is passed via a Effect to the editor.
Notice that these replace the need for a "update" method in our domain model (state machine).
We use the Structure API to update our domain model, naturally.
-}
addNewEvent : (Node Domain, Maybe (Node Domain)) -> Node Domain
addNewEvent (sm, mbEvent) =
    case mbEvent of
        Nothing ->
            addToCustom "events"
                (createNode Event
                    |> addText "name" ""
                )
                sm
    
        Just event ->
            insertAfterUnderCustom "events"
                (createNode Event
                    |> addText "name" ""
                )
                (pathOf event)
                sm


{-| This pattern allows to update nested records.
Input:
  * Tuple of
      ** domain root (state machine)
      ** path to the node we want to update
      ** key of the property
  * New property value

The actual update can be defered to the Structure module, since it knows the structure (duh!)
-}
updateStringProperty : ( Node Domain, Path, String ) -> String -> Node Domain
updateStringProperty ( sm, path, key ) newValue =
    updatePropertyByPath sm path (stringProperty ( key, newValue ))
