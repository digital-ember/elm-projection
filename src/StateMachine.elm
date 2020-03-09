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
        |> addToCustom "events"
            (createNode Event
                |> addText "name" "doorOpened"
            )
        |> Debug.log "stateMachine"


editor : Node Domain -> Node Cell
editor sm =
    createRootCell
        |> with
            (vertStackCell
                |> with
                    (constant "events")
                |> with
                    (vertStackCell 
                      |> addIndent
                      |> withRange (editorEvents sm)
                    )
                |> with
                    (constant "end")
            )


editorEvents : Node Domain -> List (Node Cell)
editorEvents sm =
    let
        mbEvents =
            getUnderCustom "events" sm
    in
        case mbEvents of
            Nothing ->
                editorEventPlaceholder

            Just events ->
                List.map editorEvent events


editorEvent : Node Domain -> Node Cell
editorEvent event =
    vertStackCell
        |> with (inputCell (propertyStringValueOf event "name" |> Maybe.withDefault ""))


editorEventPlaceholder : List (Node Cell)
editorEventPlaceholder =
    [ placeholderCell "no events" ]


main : Program () (Model Domain) Runtime.Msg
main =
    program stateMachine editor
