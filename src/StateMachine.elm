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
                |> with ( editorStateMachineName sm )

                |> with ( editorEvents sm )  

            )

editorStateMachineName : Node Domain -> Node Cell
editorStateMachineName sm =
    horizStackCell
        |> with 
          ( constant "name:" ) 
        |> with
          ( inputCell (propertyStringValueOf sm "name" |> Maybe.withDefault "") )


editorEvents : Node Domain -> Node Cell
editorEvents sm =
    let
        editorEventsResult =
            case getUnderCustom "events" sm of
                Nothing ->
                    editorEventPlaceholder

                Just events ->
                    List.map editorEvent events      
    in
        vertStackCell
            |> addIndent
            |> with
                (constant "events")
            |> with
                (vertStackCell 
                  |> addIndent
                  |> withRange editorEventsResult
                )
            |> with
                (constant "end")
        


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
