module StateMachine exposing (..)

import Structure exposing (..)
import Editor exposing (..)
import Runtime exposing (..)


type Msg
    = NoOp
    | ChangeStateMachineName (Node Domain) String
    | InsertEventAfter (Node Domain)
    | UpdateEventName (Node Domain) String
    | DeleteEvent (Node Domain)



--| MoveUpEvent Node
--| MoveDownEvent Node


type Domain
    = StateMachine
    | Event


stateMachine : Node Domain
stateMachine =
    createRoot StateMachine
        |> addText "name" "MyStateMachine"
        |> addInt "maxNumOfStates" 0
        {- |> addToCustom "events"
            (createNode Event
                |> addText "name" "doorClosed"
            )
        |> addToCustom "events"
            (createNode Event
                |> addText "name" "doorOpened"
            )-}
        |> Debug.log "stateMachine"


editor : Node Domain -> Node (Cell Domain) 
editor sm =
    createRootCell (isaOf sm)
        |> with (editorStateMachine sm)


editorStateMachine : Node Domain -> Node (Cell Domain)
editorStateMachine sm =
    vertStackCell (isaOf sm)
        |> with (editorStateMachineName sm)
        |> with (editorEvents sm)


editorStateMachineName : Node Domain -> Node (Cell Domain)
editorStateMachineName sm =
    horizStackCell (isaOf sm)
        |> with
            (constant "name:" (isaOf sm))
        |> with
            (inputCell (textOf "name" sm) (isaOf sm))


editorEvents : Node Domain -> Node (Cell Domain)
editorEvents sm =
    let
        editorEventsResult =
            case getUnderCustom "events" sm of
                Nothing ->
                    editorEventPlaceholder sm

                Just events ->
                    List.map editorEvent events
    in
        vertStackCell (isaOf sm)
            |> addIndent
            |> with
                (constant "events" (isaOf sm))
            |> with
                (vertStackCell (isaOf sm)
                    |> addIndent
                    |> withRange editorEventsResult
                )
            |> with
                (constant "end" (isaOf sm))


editorEvent : Node Domain -> Node (Cell Domain)
editorEvent event =
    vertStackCell (isaOf event)
        |> with (inputCell (textOf "name" event) (isaOf event))


editorEventPlaceholder : Node Domain -> List (Node (Cell Domain))
editorEventPlaceholder sm =
    [ placeholderCell "no events" 
        |> addToCustom "onEnter" ( onEnterEffect sm addDefaultEvent )
    ]

addDefaultEvent : Node Domain -> Node Domain
addDefaultEvent sm =
    addToCustom "events" 
      (createNode Event
          |> addText "name" "new event"
      ) sm

domainUpdate : Msg -> Node Domain -> Node Domain 
domainUpdate msg root = 
  root

main : Program () (Model Domain) (Runtime.Msg Domain)
main =
    program stateMachine editor domainUpdate
