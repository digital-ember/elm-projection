module Runtime exposing(..)

import Structure exposing(..)
import Editor exposing(..)
import Maybe exposing (..)
import Html exposing (..)
import Browser exposing (..)

type alias Model a =
    { appModel : Node a
    , xform : Node a -> (Node Cell)
    }


type Msg
    = NoOp


program : Node a -> (Node a -> Node Cell) -> Program () (Model a) Msg 
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


runtimeUpdate : Msg -> Model a -> ( Model a, Cmd Msg )
runtimeUpdate msg model =  
    ( model, Cmd.none )


view : Model a -> Html Msg 
view model =
    let
        cellRoot =
            model.xform model.appModel |> addPaths |> Debug.log "cell"
    in
        Html.map (\cellMsg -> NoOp) (viewCell cellRoot)
 

