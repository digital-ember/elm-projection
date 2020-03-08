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
        |> addToCustom "events"
            (createNode Event
                |> addText "name" "doorClosed"
            )
        |> Debug.log "stateMachine"

 
editor : Node Domain -> Node Cell
editor sm =
    createRootCell
        |> addToDefault 
          ( createNode (StackCell Vert) 
            |> addToDefault (constantCell "event")
            |> range (editorEvents sm)
            |> addToDefault (constantCell "end")
            
          )
        {- |> vertStackCell
            |> constantCell "event"
            |> range (editorEvents sm)
            |> constantCell "end"
        -}     

editorEvents : Node Domain -> List (Node Cell)
editorEvents sm =
    List.map editorEvent (getUnderCustom "events" sm |> Maybe.withDefault [])


editorEvent : Node Domain -> Node Cell
editorEvent event =
    createNode (StackCell Vert)
        |> addToDefault (inputCell (propertyStringValueOf event "name" |> Maybe.withDefault ""))

        

main : Program () (Model Domain) Runtime.Msg
main =
    program stateMachine editor
