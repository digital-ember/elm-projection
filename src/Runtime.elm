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


program : Node a -> (Node a -> Node (Cell a)) -> Program () (Model a) (Msg a)
program domainModel xform =
    let
        init () =
            ( { domainModel = domainModel |> updatePaths, xform = xform }, Cmd.none )
           
    in
        Browser.element
            { init = init
            , update = update
            , view = view
            , subscriptions = \_ -> Sub.none
            }


update : Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        EditorMsg eMsg ->
            let
                ( domainModelNew, editorCmd ) =
                    updateEditor eMsg model.domainModel
            in
                ( { model | domainModel = domainModelNew }, Cmd.map (\editorMsg -> EditorMsg editorMsg) editorCmd )


view : Model a -> Html (Msg a)
view model =
    let
        cellRoot =
            model.xform model.domainModel |> updatePaths |> Debug.log "cell"
    in
        Html.map (\editorMsg -> EditorMsg editorMsg) (viewEditor cellRoot)



{-
cell =
       Node
           { features =
               { custom = Dict.fromList []
               , default =
                   Just
                       [ Node
                           { features =
                               { custom = Dict.fromList []
                               , default =
                                   Just
                                       [ Node
                                           { features =
                                               { custom = Dict.fromList []
                                               , default =
                                                   Just
                                                       [ Node
                                                           { features =
                                                               { custom = Dict.fromList [], default = Nothing }
                                                           , isa = ContentCell ConstantCell
                                                           , name = ""
                                                           , path = "root:default0:default0:default0"
                                                           , properties = Dict.fromList [ ( "constant", PString "name:" ) ]
                                                           }
                                                       , Node
                                                           { features =
                                                               { custom = Dict.fromList [], default = Nothing }
                                                           , isa = ContentCell InputCell
                                                           , name = ""
                                                           , path = "root:default0:default0:default1"
                                                           , properties = Dict.fromList [ ( "input", PString "" ) ]
                                                           }
                                                       ]
                                               }
                                           , isa = ContentCell (StackCell Horiz)
                                           , name = ""
                                           , path = "root:default0:default0"
                                           , properties = Dict.fromList []
                                           }
                                       , Node
                                           { features =
                                               { custom = Dict.fromList []
                                               , default =
                                                   Just
                                                       [ Node
                                                           { features =
                                                               { custom = Dict.fromList [], default = Nothing }
                                                           , isa = ContentCell ConstantCell
                                                           , name = ""
                                                           , path = "root:default0:default1:default0"
                                                           , properties = Dict.fromList [ ( "constant", PString "events" ) ]
                                                           }
                                                       , Node
                                                           { features =
                                                               { custom = Dict.fromList []
                                                               , default =
                                                                   Just
                                                                       [ Node
                                                                           { features =
                                                                               { custom =
                                                                                   Dict.fromList
                                                                                       [ ( "effects"
                                                                                         , [ Node
                                                                                               { features =
                                                                                                   { custom = Dict.fromList [], default = Nothing }
                                                                                               , isa =
                                                                                                   EffectCell
                                                                                                       (OnEnterEffect
                                                                                                           { effectHandler = "<function>"
                                                                                                           , effectInput =
                                                                                                               Node
                                                                                                                   { features =
                                                                                                                       { custom =
                                                                                                                           Dict.fromList
                                                                                                                               [ ( "events"
                                                                                                                                 , [ Node
                                                                                                                                       { features =
                                                                                                                                           { custom = Dict.fromList [], default = Nothing }
                                                                                                                                       , isa = Event
                                                                                                                                       , name = ""
                                                                                                                                       , path = "root:events0"
                                                                                                                                       , properties = Dict.fromList [ ( "name", PString "" ) ]
                                                                                                                                       }
                                                                                                                                   ]
                                                                                                                                 )
                                                                                                                               ]
                                                                                                                       , default = Nothing
                                                                                                                       }
                                                                                                                   , isa = StateMachine
                                                                                                                   , name = "root"
                                                                                                                   , path = "root"
                                                                                                                   , properties = Dict.fromList []
                                                                                                                   }
                                                                                                           }
                                                                                                       )
                                                                                               , name = ""
                                                                                               , path = "root:default0:default1:default1:default0:effects0"
                                                                                               , properties = Dict.fromList []
                                                                                               }
                                                                                           ]
                                                                                         )
                                                                                       ]
                                                                               , default = Nothing
                                                                               }
                                                                           , isa = ContentCell PlaceholderCell
                                                                           , name = ""
                                                                           , path = "root:default0:default1:default1:default0"
                                                                           , properties = Dict.fromList [ ( "placeholder", PString "no events" ) ]
                                                                           }
                                                                       ]
                                                               }
                                                           , isa = ContentCell (StackCell Vert)
                                                           , name = ""
                                                           , path = "root:default0:default1:default1"
                                                           , properties = Dict.fromList [ ( "indent", PBool True ) ]
                                                           }
                                                       , Node
                                                           { features =
                                                               { custom = Dict.fromList [], default = Nothing }
                                                           , isa = ContentCell ConstantCell
                                                           , name = ""
                                                           , path = "root:default0:default1:default2"
                                                           , properties = Dict.fromList [ ( "constant", PString "end" ) ]
                                                           }
                                                       ]
                                               }
                                           , isa = ContentCell (StackCell Vert)
                                           , name = ""
                                           , path = "root:default0:default1"
                                           , properties = Dict.fromList []
                                           }
                                       ]
                               }
                           , isa = ContentCell (StackCell Vert)
                           , name = ""
                           , path = "root:default0"
                           , properties = Dict.fromList []
                           }
                       ]
               }
           , isa = ContentCell RootCell
           , name = "root"
           , path = "root"
           , properties = Dict.fromList []
           }
-}