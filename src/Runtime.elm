module Runtime exposing (..)

import Structure exposing (..)
import Editor exposing (..)
import Maybe exposing (..)
import Html exposing (..)
import Browser exposing (..)


type alias Model a =
    { domainModel : Node a
    , xform : Node a -> Node (Cell a)
    }


type Msg a
    = NoOp
    | EditorMsg (Editor.Msg a)


program : Node a -> (Node a -> Node (Cell a)) -> (domainMsg -> Node a -> Node a) -> Program () (Model a) (Msg a)
program domainModel xform domainUpdate =
    let
        init () =
            ( { domainModel = domainModel, xform = xform }, Cmd.none )

        update msg model =
            runtimeUpdate domainUpdate msg model
    in
        Browser.element
            { init = init
            , update = update
            , view = view
            , subscriptions = \_ -> Sub.none
            }


runtimeUpdate : (domainMsg -> Node a -> Node a) -> Msg a -> Model a -> ( Model a, Cmd (Msg a) )
runtimeUpdate domainUpdate msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        EditorMsg eMsg ->
            let
                ( domainModelNew, editorCmd ) =
                    editorUpdate domainUpdate eMsg model.domainModel
            in
                ( { model | domainModel = domainModelNew }, Cmd.map (\editorMsg -> EditorMsg editorMsg) editorCmd )


view : Model a -> Html (Msg a)
view model =
    let
        cellRoot =
            model.xform model.domainModel |> addPaths |> Debug.log "cell"
    in
        Html.map (\editorMsg -> EditorMsg editorMsg) (viewRoot cellRoot)
