module StateMachine exposing(..)

import Structure exposing(..)
import Editor exposing(..)
import Runtime exposing(..)

type Msg
    = NoOp
    | ChangeStateMachineName Node String
    | InsertEventAfter Node
    | UpdateEventName Node String
    | DeleteEvent Node
    --| MoveUpEvent Node
    --| MoveDownEvent Node


stateMachine : Node
stateMachine =
    createRoot "StateMachine"
        |> addText "name" "MyStateMachine"
        |> addInt "maxNumOfStates" 0
        |> underCustom "events" 
            ( createNode "Event"
                |> addText "name" "doorClosed"
            )
        |> addPaths 
        |> Debug.log "stateMachine"


editor : Node -> Node
editor node = 
    rootCell
        (propertiesOf node |> List.map xformStateMachineProperties)
        (featuresOf node |> List.map xformStateMachineTraits)


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


xformStateMachineTraits : Feature -> Feature
xformStateMachineTraits trait =
    Debug.todo "todo"
   {- case trait of
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