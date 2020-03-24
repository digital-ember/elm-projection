module Statemachine exposing (main)

import Editor as E
import Runtime as R
import Structure as S



-- DOMAIN TYPE


type Domain
    = Statemachine
    | Event
    | State
    | Transition


main : Program () (R.Model Domain) (R.Msg Domain)
main =
    R.projection emptyStatemachine editor


emptyStatemachine : S.Node Domain
emptyStatemachine =
    S.createRoot Statemachine



-- EDITOR


editor : S.Node Domain -> S.Node (E.Cell Domain)
editor sm =
    E.rootCell
        |> E.with (editorStatemachine sm)


editorStatemachine : S.Node Domain -> S.Node (E.Cell Domain)
editorStatemachine sm =
    E.vertSplitCell
        |> E.with
            (E.vertStackCell
                |> E.with (editorStatemachineName sm)
                |> E.with (editorEvents sm)
                |> E.with (editorStates sm)
            )
        |> E.with
            (E.graphCell
                |> E.withRange (editorStatesVerticies sm)
                |> E.withRange (editorTransitionsEdges sm)
            )


editorStatemachineName : S.Node Domain -> S.Node (E.Cell Domain)
editorStatemachineName sm =
    E.horizStackCell
        |> E.with (E.constantCell "name:")
        |> E.with (E.inputCell S.roleName sm)
        |> E.addMargin E.Bottom 20


editorEvents : S.Node Domain -> S.Node (E.Cell Domain)
editorEvents sm =
    let
        editorEventsResult =
            case S.getUnderCustom roleEvents sm of
                [] ->
                    [ editorEventPlaceholder sm ]

                events ->
                    List.map editorEvent events
    in
    E.vertStackCell
        |> E.with (E.constantCell "events")
        |> E.with
            (E.vertStackCell
                |> E.addIndent
                |> E.withRange editorEventsResult
            )
        |> E.with (E.constantCell "end")
        |> E.addMargin E.Bottom 20


editorEvent : S.Node Domain -> S.Node (E.Cell Domain)
editorEvent event =
    E.inputCell S.roleName event
        |> E.withEffect (E.insertionEffect event ctorEvent)
        |> E.withEffect (E.deletionEffect event)


editorEventPlaceholder : S.Node Domain -> S.Node (E.Cell Domain)
editorEventPlaceholder sm =
    E.placeholderCell "no events"
        |> E.withEffect (E.replacementEffect roleEvents sm ctorEvent)


editorStates : S.Node Domain -> S.Node (E.Cell Domain)
editorStates sm =
    let
        editorStatesResult =
            case S.getUnderDefault sm of
                [] ->
                    [ editorStatesPlaceholder sm ]

                states ->
                    List.map editorState states
    in
    E.vertStackCell
        |> E.withRange editorStatesResult


editorStatesPlaceholder : S.Node Domain -> S.Node (E.Cell Domain)
editorStatesPlaceholder sm =
    E.placeholderCell "no states"
        |> E.withEffect (E.replacementEffect S.roleDefault sm ctorState)


editorState : S.Node Domain -> S.Node (E.Cell Domain)
editorState state =
    let
        editorTransitionsResult =
            case S.getUnderDefault state of
                [] ->
                    [ editorTransitionPlaceholder state ]

                transitions ->
                    List.map editorTransition transitions
    in
    E.vertStackCell
        |> E.with (editorStateHead state)
        |> E.with
            (E.vertStackCell
                |> E.addIndent
                |> E.withRange editorTransitionsResult
            )
        |> E.with (E.constantCell "end")
        |> E.with
            (E.buttonCell "+"
                |> E.withEffect (E.insertionEffect state ctorState)
            )
        |> E.addMargin E.Bottom 20


editorStateHead : S.Node Domain -> S.Node (E.Cell Domain)
editorStateHead state =
    E.horizStackCell
        |> E.with (E.constantCell "state")
        |> E.with
            (E.inputCell S.roleName state
                |> E.withEffect (E.insertionEffect state ctorState)
                |> E.withEffect (E.deletionEffect state)
            )


editorTransitionPlaceholder : S.Node Domain -> S.Node (E.Cell Domain)
editorTransitionPlaceholder state =
    E.placeholderCell "no transitions"
        |> E.withEffect (E.replacementEffect S.roleDefault state ctorTransition)


editorTransition : S.Node Domain -> S.Node (E.Cell Domain)
editorTransition transition =
    E.horizStackCell
        |> E.with
            (E.refCell Event roleEventRef transition Nothing
                |> E.withEffect (E.insertionEffect transition ctorTransition)
                |> E.withEffect (E.deletionEffect transition)
            )
        |> E.with (E.constantCell "⇒")
        |> E.with
            (E.refCell State roleStateRef transition Nothing
                |> E.withEffect (E.insertionEffect transition ctorTransition)
                |> E.withEffect (E.deletionEffect transition)
            )


editorStatesVerticies : S.Node Domain -> List (S.Node (E.Cell Domain))
editorStatesVerticies sm =
    List.map editorStateVertex <| S.getUnderDefault sm


editorStateVertex state =
    E.vertexCell S.roleName state


editorTransitionsEdges : S.Node Domain -> List (S.Node (E.Cell Domain))
editorTransitionsEdges sm =
    S.getUnderDefault sm
        |> List.map editorTransitionEdge
        |> List.concat


editorTransitionEdge state =
    let
        sourceName =
            S.textOf S.roleName state

        edge transition =
            let
                targetName =
                    S.textOf roleStateRef transition
            in
            E.edgeCell roleEventRef ( sourceName, targetName ) transition
    in
    S.getUnderDefault state
        |> List.map edge



-- CTORs


ctorEvent : S.Node Domain
ctorEvent =
    S.createNode Event
        |> S.addText S.roleName ""


ctorState : S.Node Domain
ctorState =
    S.createNode State
        |> S.addText S.roleName ""


ctorTransition : S.Node Domain
ctorTransition =
    S.createNode Transition
        |> S.addText roleEventRef ""
        |> S.addText roleStateRef ""



-- ROLES


roleEventRef =
    S.roleFromString "eventRef"


roleStateRef =
    S.roleFromString "stateRef"


roleEvents =
    S.roleFromString "events"



-- DEBUGGING STUFF


mrsHsSecretCompartmentDemo : S.Node Domain
mrsHsSecretCompartmentDemo =
    S.createRoot Statemachine
        |> S.addText S.roleName "Mrs H's secret compartment system"
        |> S.addRangeToCustom roleEvents
            [ S.createNode Event |> S.addText S.roleName "doorClosed"
            , S.createNode Event |> S.addText S.roleName "drawOpened"
            , S.createNode Event |> S.addText S.roleName "lightOn"
            , S.createNode Event |> S.addText S.roleName "doorOpened"
            , S.createNode Event |> S.addText S.roleName "panelClosed"
            ]


mrsHsSecretCompartment : S.Node Domain
mrsHsSecretCompartment =
    S.createRoot Statemachine
        |> S.addText S.roleName "Mrs H's secret compartment system"
        |> S.addRangeToCustom roleEvents
            [ S.createNode Event |> S.addText S.roleName "doorClosed"
            , S.createNode Event |> S.addText S.roleName "drawOpened"
            , S.createNode Event |> S.addText S.roleName "lightOn"
            , S.createNode Event |> S.addText S.roleName "doorOpened"
            , S.createNode Event |> S.addText S.roleName "panelClosed"
            ]
        |> S.addRangeToDefault
            [ S.createNode State
                |> S.addText S.roleName "idle"
                |> S.addToDefault
                    (S.createNode Transition
                        |> S.addText roleEventRef "doorClosed"
                        |> S.addText roleStateRef "active"
                    )
            , S.createNode State
                |> S.addText S.roleName "active"
                |> S.addToDefault
                    (S.createNode Transition
                        |> S.addText roleEventRef "drawOpened"
                        |> S.addText roleStateRef "waitingForLight"
                    )
                |> S.addToDefault
                    (S.createNode Transition
                        |> S.addText roleEventRef "lightOn"
                        |> S.addText roleStateRef "waitingForDraw"
                    )
            , S.createNode State
                |> S.addText S.roleName "waitingForLight"
                |> S.addToDefault
                    (S.createNode Transition
                        |> S.addText roleEventRef "lightOn"
                        |> S.addText roleStateRef "unlockedPanel"
                    )
            , S.createNode State
                |> S.addText S.roleName "waitingForDraw"
                |> S.addToDefault
                    (S.createNode Transition
                        |> S.addText roleEventRef "drawOpened"
                        |> S.addText roleStateRef "unlockedPanel"
                    )
            , S.createNode State
                |> S.addText S.roleName "unlockedPanel"
            ]