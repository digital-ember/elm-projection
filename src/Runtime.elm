module Runtime exposing
    ( Model
    , Msg
    , projection
    )

import Browser exposing (..)
import Editor exposing (..)
import Html exposing (..)
import Structure exposing (..)


type alias Model a =
    { domainD : Domain a (Node (Cell a))
    , domainE : Domain (Cell a) (Html (Editor.Msg a))
    }


type alias Domain a b =
    { root : Node a
    , xform : Node a -> b
    }


type Msg a
    = EditorMsg (Editor.Msg a)


projection : Node a -> (Node a -> Node (Cell a)) -> Program () (Model a) (Msg a)
projection rootD xform =
    let
        init () =
            ( { domainD = Domain (rootD |> updatePaths) xform
              , domainE = Domain (xform rootD |> griddify |> updatePaths) viewEditor
              }
            , Cmd.none
            )
    in
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


update : Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update msg ({ domainD, domainE } as model) =
    case msg of
        EditorMsg eMsg ->
            let
                ( rootDNew, editorCmd ) =
                    updateEditor eMsg domainE.root domainD.root

                domainDNew =
                    { domainD | root = rootDNew }

                rootENew =
                    runDomainXform domainDNew

                domainENew =
                    { domainE | root = rootENew }

            in
            ( { model | domainD = domainDNew, domainE = domainENew }, Cmd.map EditorMsg editorCmd )


view : Model a -> Html (Msg a)
view model =
    Html.map EditorMsg (model.domainE.xform model.domainE.root)


runDomainXform domainD =
    domainD.xform domainD.root |> griddify |> updatePaths
