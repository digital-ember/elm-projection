module Runtime exposing(..)

import Structure exposing(..)
import Editor exposing(..)
import Maybe exposing (..)
import Html exposing (..)
import Browser exposing (..)

type alias Model =
    { appModel : Node
    , xform : Node -> Node
    }


type Msg
    = NoOp


program : Node -> (Node -> Node) -> Program () Model Msg 
program appModel xform =
    let
        init () =
            ( { appModel = appModel, xform = xform }, Cmd.none )

        update msg model =
            runtimeUpdate msg model
    in
        Browser.element
            { init = init
            , update = update
            , view = view
            , subscriptions = \_ -> Sub.none
            }


runtimeUpdate : Msg -> Model -> ( Model, Cmd Msg )
runtimeUpdate msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        cellRoot =
            model.xform model.appModel |> Debug.log "cell"
    in
        Html.map (\cellMsg -> NoOp) (viewCell cellRoot)
 

