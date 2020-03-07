module StateMachine exposing (..)

import Structure exposing (..)
import Editor exposing (..)
import Runtime exposing (..)


type Msg
    = NoOp
    | ChangeStateMachineName (Node StateMachine) String
    | InsertEventAfter (Node Event)
    | UpdateEventName (Node Event) String
    | DeleteEvent (Node Event)



--| MoveUpEvent Node
--| MoveDownEvent Node


type StateMachine
    = StateMachine


type Event
    = Event


stateMachine : Node StateMachine
stateMachine =
    createRoot StateMachine
        |> addText "name" "MyStateMachine"
        |> addInt "maxNumOfStates" 0
        |> addToCustom "events"
            (createNode "Event" Event
                |> addText "name" "doorClosed"
            )
        |> Debug.log "stateMachine"


editor : Node StateMachine -> Node Cell
editor sm =
    createRootCell
        |> vertStackCell
            |> constantCell "event"
            |> range (editorEvents sm)
            |> constantCell "end"
             

editorEvents : Node StateMachine -> List (Node Cell)
editorEvents sm =
    List.map editorEvent (getUnderCustom "events" sm |> Maybe.withDefault [])


editorEvent : Node Event -> Node Cell
editorEvent event =
    inputCell (propertyStringValueOf event "name" |> Maybe.withDefault "")


main : Program () (Model StateMachine) Runtime.Msg
main =
    program stateMachine editor
