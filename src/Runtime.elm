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
    | DragAt (Editor.Msg a)
    | DragEnd (Editor.Msg a)


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
    case model.editorModel.drag of
        Nothing ->
            case model.editorModel.mbSimulation of
                -- this trick makes it so to fire a animation every time the domain model changes
                Just simulation ->
                    if Force.isCompleted simulation then
                        Sub.none

                    else
                        Browser.Events.onAnimationFrame (Tick Editor.Tick)

                Nothing ->
                    Browser.Events.onAnimationFrame (Tick Editor.Tick)

        Just _ ->
            Sub.batch
                [ Browser.Events.onMouseMove
                    (JsonD.map
                        (\mEvent ->
                            let
                                d =
                                    Debug.log "offsetPos" mEvent.offsetPos
                                e =
                                    Debug.log "screenClient" mEvent.clientPos
                            in
                                
                            DragAt (Editor.DragAt (Tuple.first mEvent.clientPos - 352, Tuple.second mEvent.clientPos ))
                        )
                        Mouse.eventDecoder
                    )
                , Browser.Events.onMouseUp (JsonD.map (\mEvent -> DragEnd (Editor.DragEnd  (Tuple.first mEvent.clientPos - 352, Tuple.second mEvent.clientPos ))) Mouse.eventDecoder)

                , Browser.Events.onAnimationFrame (Tick Editor.Tick)
                ]


type alias MouseData =
    { clientX : Float
    , clientY : Float
    , offsetTop : Float
    , offsetLeft : Float
    }


decoder : Decoder MouseData
decoder =
    JsonD.map4 MouseData
        (JsonD.at [ "clientX" ] JsonD.float)
        (JsonD.at [ "clientY" ] JsonD.float)
        (JsonD.at [ "target", "offsetTop" ] JsonD.float)
        (JsonD.at [ "target", "offsetLeft" ] JsonD.float)


update : Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update msg ({ domain, editorModel } as model) =
    case msg of
        Tick eMsg _ ->
            let
                ( runXform, editorModelUpdated, editorCmd ) =
                    updateEditor eMsg editorModel
            in
            ( { model | editorModel = editorModelUpdated }, Cmd.none )

        DragAt eMsg ->
            let
                ( runXform, editorModelUpdated, editorCmd ) =
                    updateEditor eMsg editorModel
            in
            ( { model | editorModel = editorModelUpdated }, Cmd.none )

        DragEnd eMsg ->
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
