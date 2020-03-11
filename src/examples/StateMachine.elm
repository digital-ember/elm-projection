module StateMachine exposing(main)

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


{-| Declarative way of building a "state machine editor" using the Editor-API
-}
editor : Node Domain -> Node (Cell Domain)
editor sm =
    createRootCell
        |> with (editorStateMachine sm)


editorStateMachine : Node Domain -> Node (Cell Domain)
editorStateMachine sm =
    vertStackCell
        |> with (editorStateMachineName sm)
        |> with (editorEvents sm)


editorStateMachineName : Node Domain -> Node (Cell Domain)
editorStateMachineName sm =
    horizStackCell
        |> with
            (constantCell "name:")
        |> with
            (inputCell (textOf "name" sm)
                |> withEffect (onInputEffect ( sm, pathOf sm ) updateName)
            )


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
        |> withEffect (onEnterEffect sm (insertNewEvent event))
        |> withEffect (onInputEffect ( sm, pathOf event ) updateName)


editorEventPlaceholder : Node Domain -> List (Node (Cell Domain))
editorEventPlaceholder sm =
    [ placeholderCell "no events"
        |> withEffect (onEnterEffect sm addDefaultEvent)
    ]


addDefaultEvent : Node Domain -> Node Domain
addDefaultEvent sm =
    addToCustom "events"
        (createNode Event
            |> addText "name" ""
        )
        sm


insertNewEvent : Node Domain -> Node Domain -> Node Domain
insertNewEvent event sm =
    insertAfterUnderCustom "events"
        (createNode Event
            |> addText "name" ""
        )
        (pathOf event)
        sm


updateName : ( Node Domain, Path ) -> String -> Node Domain
updateName ( sm, path ) newName =
    updatePropertyByPath sm path (stringProperty ("name", newName))



