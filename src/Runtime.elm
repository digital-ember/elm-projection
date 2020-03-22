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


projection : Node a -> (Node a -> Node (Cell a)) -> Program () (Model a) (Msg a)
projection rootD xform =
    let
        init () =
            ( { domain = Domain (rootD |> updatePaths) xform
              , editorModel = initEditorModel rootD (xform rootD |> griddify |> updatePaths)
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
    case model.editorModel.mbSimulation of
        Just simulation ->
            if Force.isCompleted simulation then
                Sub.none

            else
                Browser.Events.onAnimationFrame (Tick Editor.Tick)

        Nothing ->
            Browser.Events.onAnimationFrame (Tick Editor.Tick)


update : Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update msg ({ domain, editorModel } as model) =
    case msg of
        Tick eMsg _ ->
            let
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
                        model
            in
            ( modelNew, Cmd.map EditorMsg editorCmd )


view : Model a -> Html (Msg a)
view model =
    Html.map EditorMsg (viewEditor model.editorModel.eRoot)


runDomainXform domain =
    domain.xform domain.root |> griddify |> updatePaths



