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
import Html.Events.Extra.Mouse as Mouse exposing (Event)
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

        dragSubs =
            case model.editorModel.drag of
                Nothing ->
                    case model.editorModel.mbSimulation of
                        -- this trick makes it so to fire a animation every time the domain model changes
                        Just simulation ->
                            if Force.isCompleted simulation then
                                []

                            else
                                [ Browser.Events.onAnimationFrame (Tick Editor.Tick) ]

                        Nothing ->
                            [ Browser.Events.onAnimationFrame (Tick Editor.Tick) ]

                Just _ ->
                    [ Browser.Events.onMouseUp
                        (JsonD.map (\mpos -> MouseUp (Editor.MouseUp mpos)) mousePosition)
                    , Browser.Events.onAnimationFrame (Tick Editor.Tick)
                    ]
    in
    Sub.batch
        (mouseMoveSub :: dragSubs)


update : Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update msg ({ domain, editorModel } as model) =
    case msg of
        Tick eMsg _ ->
            let
                ( runXform, editorModelUpdated, editorCmd ) =
                    updateEditor eMsg editorModel
            in
            ( { model | editorModel = editorModelUpdated }, Cmd.none )

        MouseMove eMsg ->
            let
                ( runXform, editorModelUpdated, editorCmd ) =
                    updateEditor eMsg editorModel
            in
            ( { model | editorModel = editorModelUpdated }, Cmd.none )

        MouseUp eMsg ->
            let
                d =
                    Debug.log "dragEnd" eMsg

                ( runXform, editorModelUpdated, editorCmd ) =
                    updateEditor eMsg editorModel
            in
            ( { model | editorModel = editorModelUpdated }, Cmd.none )

        EditorMsg eMsg ->
            let
                ( runXform, editorModelUpdated, editorCmd ) =
                    updateEditor eMsg editorModel

                modelNew =
                    if runXform then
                        let
                            domainNew =
                                { domain | root = editorModelUpdated.dRoot }

                            rootENew =
                                runDomainXform domainNew

                            editorModelNew =
                                { editorModelUpdated | eRoot = rootENew, mbSimulation = Nothing }
                        in
                        { model | domain = domainNew, editorModel = editorModelNew }

                    else
                        { model | editorModel = editorModelUpdated }
            in
            ( modelNew, Cmd.map EditorMsg editorCmd )


view : Model a -> Html (Msg a)
view model =
    Html.map (\eMsg -> EditorMsg eMsg |> Debug.log "receiving") (viewEditor model.editorModel.eRoot)


runDomainXform domain =
    domain.xform domain.root |> griddify |> updatePaths
