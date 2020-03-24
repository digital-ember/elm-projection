module Runtime exposing
    ( Model
    , Msg
    , projection
    )

import Browser exposing (..)
import Browser.Events
import Editor exposing (..)
import Force
import Html exposing (..)
import Json.Decode as JsonD exposing (Decoder)
import Structure exposing (..)
import Time


type alias Model a =
    { domain : Domain a (Cell a)
    , editorModel : EditorModel a
    }


type alias Domain a b =
    { root : Node a
    , xform : Node a -> Node b
    }


type Msg a
    = EditorMsg (Editor.Msg a)
    | Tick (Editor.Msg a) Time.Posix
    | MouseUp (Editor.Msg a)
    | MouseMove (Editor.Msg a)


projection : Node a -> (Node a -> Node (Cell a)) -> Program () (Model a) (Msg a)
projection rootD xform =
    let
        rootDWithPaths =
            rootD |> updatePaths

        init () =
            ( { domain = Domain rootDWithPaths xform
              , editorModel = initEditorModel rootDWithPaths (xform rootDWithPaths |> griddify |> updatePaths)
              }
            , Cmd.none
            )
    in
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model a -> Sub (Msg a)
subscriptions model =
    let
        mouseMoveSub =
            Browser.Events.onMouseMove
                (JsonD.map (\mpos -> MouseMove (Editor.MouseMove mpos)) mousePosition)

        graphSubs =
            case model.editorModel.drag of
                Nothing ->
                    case model.editorModel.mbSimulation of
                        Just simulation ->
                            if Force.isCompleted simulation then
                                []

                            else
                                [ Browser.Events.onAnimationFrame (Tick Editor.Tick) ]

                        Nothing ->
                            if model.editorModel.runSimulation then
                                [ Browser.Events.onAnimationFrame (Tick Editor.Tick) ]

                            else
                                []

                Just _ ->
                    [ Browser.Events.onMouseUp
                        (JsonD.map (\mpos -> MouseUp (Editor.MouseUp mpos)) mousePosition)
                    , Browser.Events.onAnimationFrame (Tick Editor.Tick)
                    ]
    in
    Sub.batch
        (mouseMoveSub :: graphSubs)


update : Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update msg ({ domain, editorModel } as model) =
    let
        updateEditorOnly eMsg =
            let
                ( editorModelUpdated, editorCmd ) =
                    updateEditor eMsg model.editorModel
            in
            ( { model | editorModel = editorModelUpdated }, Cmd.map EditorMsg editorCmd )
    in
    case msg of
        Tick eMsg _ ->
            updateEditorOnly eMsg

        MouseMove eMsg ->
            updateEditorOnly eMsg

        MouseUp eMsg ->
            updateEditorOnly eMsg

        EditorMsg eMsg ->
            let
                ( editorModelUpdated, editorCmd ) =
                    updateEditor eMsg editorModel

                updateSimul graphsDiffer simul =
                    if Force.isCompleted simul then
                        Nothing

                    else if graphsDiffer then
                        Nothing

                    else
                        Just simul

                modelNew =
                    if editorModelUpdated.runXform then
                        let
                            domainNew =
                                { domain | root = editorModelUpdated.dRoot }

                            rootENew =
                                runDomainXform domainNew
                                    |> persistVertexPositions editorModelUpdated.eRoot

                            graphsDiffer =
                                graphComparer editorModel.eRoot rootENew == False

                            mbSimulNew =
                                editorModelUpdated.mbSimulation
                                    |> Maybe.andThen (updateSimul graphsDiffer)

                            editorModelNew =
                                { editorModelUpdated | eRoot = rootENew, mbSimulation = mbSimulNew, runSimulation = graphsDiffer }
                        in
                        { model | domain = domainNew, editorModel = editorModelNew }

                    else
                        let
                            rootENew =
                                persistVertexPositions editorModelUpdated.eRoot editorModelUpdated.eRoot

                            graphsDiffer =
                                graphComparer editorModel.eRoot rootENew == False

                            mbSimulNew =
                                editorModelUpdated.mbSimulation
                                    |> Maybe.andThen (updateSimul graphsDiffer)

                            editorModelNew =
                                { editorModelUpdated | eRoot = rootENew, mbSimulation = mbSimulNew }
                        in
                        { model | editorModel = editorModelNew }
            in
            ( modelNew, Cmd.map EditorMsg editorCmd )


view : Model a -> Html (Msg a)
view model =
    Html.map (\eMsg -> EditorMsg eMsg) (viewEditor model.editorModel.eRoot)


runDomainXform domain =
    domain.xform domain.root |> griddify |> updatePaths


