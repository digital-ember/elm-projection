module StateMachine exposing(..)

import Structure exposing(..)
import Editor exposing(..)
import Runtime exposing(..)

type Msg
    = NoOp
    | ChangeStateMachineName (Node StateMachine) String
    | InsertEventAfter (Node Event)
    | UpdateEventName (Node Event) String
    | DeleteEvent (Node Event)
    --| MoveUpEvent Node
    --| MoveDownEvent Node


type StateMachine = StateMachine

type Event = Event


stateMachine : Node StateMachine
stateMachine =
    createRoot StateMachine
        |> addText "name" "MyStateMachine"
        |> addInt "maxNumOfStates" 0
        |> underCustom "events" 
            ( createNode Event
                |> addText "name" "doorClosed"
            )
        |> addPaths 
        |> Debug.log "stateMachine"


editor : Node StateMachine -> Node Cell 
editor sm = 
    rootCell [] []
      |> underCustom "events"
          ( vertStackCell [] [] 
              |> underCustom "event"
                ( constantCell [] "event" )
              
              |> underMainRange 
                ( List.map editorEvent (getUnderCustom "root:events" sm) )

              |> underCustom "end"
                ( constantCell [] "end" )
          )

 
xformStateMachineProperties : Property -> Property
xformStateMachineProperties property =
    Debug.todo "todo"
    {-
    case property of
        StringProperty "name" value -> 
            StringProperty 

        IntProperty "numOfStates" value ->
            Mandatory "numOfStates" <| constantCell [] (String.fromInt value)

        _ ->
            None -}


editorEvent : Node Event -> Node Cell
editorEvent event =
    let
      text = propertyStringValueOf event "name" |> Maybe.withDefault ""
    in
      inputCell [] text
    
{-
     case trait of
        OneToN "states" states ->
            ZeroToN "states"
                [ vertStackCell []
                    [-- map xformState
                    ]
                ]

        ZeroToN "events" events ->
            ZeroToN "events"
                [ vertStackCell []
                    [ Mandatory "eventsKeyword" <|
                        constantCell [] "events"
                    , OneToN "main" <|
                        [ vertStackCell []
                            [-- map eventName
                            ]
                        ]
                    , Mandatory "endKeyword" <|
                        constantCell [] "end"
                    ]
                ]

        Mandatory "startState" startState ->
            None

        _ ->
            None
-}

main =
    program stateMachine editor