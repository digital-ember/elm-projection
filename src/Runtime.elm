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
    { root: Node a
    , xform : Node a -> b
    }


type Msg a
    = NoOp
    | EditorMsg (Editor.Msg a)


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
update msg ({domainD, domainE} as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        EditorMsg eMsg ->
            let
                ( rootDNew, editorCmd ) =
                    updateEditor eMsg domainE.root domainD.root

                rootENew =
                    runDomainXform domainD

                domainDNew = { domainD | root = rootDNew }

                domainCNew = { domainE | root = rootENew }
            in
            ( { model | domainD = domainDNew, domainE = domainCNew }, Cmd.map EditorMsg editorCmd )


view : Model a -> Html (Msg a)
view model =
    Html.map EditorMsg (model.domainE.xform model.domainE.root)


runDomainXform domainD =
    domainD.xform domainD.root |> griddify |> updatePaths



{-
   model =
       Node
           { features =
               { custom =
                   Dict.fromList
                       [ ( "events"
                         , [ Node
                               { features =
                                   { custom = Dict.fromList []
                                   , default = Nothing
                                   }
                               , isa = Event
                               , name = ""
                               , path =
                                   Path
                                       [ { feature = "root"
                                         , index = 0
                                         }
                                       , { feature = "events"
                                         , index = 0
                                         }
                                       ]
                               , properties = Dict.fromList [ ( "name", PString "doorClosed" ) ]
                               }
                           , Node
                               { features =
                                   { custom = Dict.fromList []
                                   , default = Nothing
                                   }
                               , isa = Event
                               , name = ""
                               , path =
                                   Path
                                       [ { feature = "root"
                                         , index = 0
                                         }
                                       , { feature = "events"
                                         , index = 1
                                         }
                                       ]
                               , properties = Dict.fromList [ ( "name", PString "drawOpenend" ) ]
                               }
                           , Node
                               { features =
                                   { custom = Dict.fromList []
                                   , default = Nothing
                                   }
                               , isa = Event
                               , name = ""
                               , path =
                                   Path
                                       [ { feature = "root"
                                         , index = 0
                                         }
                                       , { feature = "events"
                                         , index = 2
                                         }
                                       ]
                               , properties = Dict.fromList [ ( "name", PString "lightOn" ) ]
                               }
                           , Node
                               { features =
                                   { custom = Dict.fromList []
                                   , default = Nothing
                                   }
                               , isa = Event
                               , name = ""
                               , path =
                                   Path
                                       [ { feature = "root"
                                         , index = 0
                                         }
                                       , { feature = "events"
                                         , index = 3
                                         }
                                       ]
                               , properties = Dict.fromList [ ( "name", PString "doorOpened" ) ]
                               }
                           , Node
                               { features =
                                   { custom = Dict.fromList []
                                   , default = Nothing
                                   }
                               , isa = Event
                               , name = ""
                               , path =
                                   Path
                                       [ { feature = "root"
                                         , index = 0
                                         }
                                       , { feature = "events"
                                         , index = 4
                                         }
                                       ]
                               , properties = Dict.fromList [ ( "name", PString "panelClosed" ) ]
                               }
                           ]
                         )
                       ]
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
                                               , default = Nothing
                                               }
                                           , isa = Transition
                                           , name = ""
                                           , path =
                                               Path
                                                   [ { feature = "root"
                                                     , index = 0
                                                     }
                                                   , { feature = "default"
                                                     , index = 0
                                                     }
                                                   , { feature = "default"
                                                     , index = 0
                                                     }
                                                   ]
                                           , properties = Dict.fromList [ ( "eventRef", PString "doorClosed" ), ( "stateRef", PString "active" ) ]
                                           }
                                       ]
                               }
                           , isa = State
                           , name = ""
                           , path =
                               Path
                                   [ { feature = "root"
                                     , index = 0
                                     }
                                   , { feature = "default"
                                     , index = 0
                                     }
                                   ]
                           , properties = Dict.fromList [ ( "name", PString "idle" ) ]
                           }
                       , Node
                           { features =
                               { custom = Dict.fromList []
                               , default =
                                   Just
                                       [ Node
                                           { features =
                                               { custom = Dict.fromList []
                                               , default = Nothing
                                               }
                                           , isa = Transition
                                           , name = ""
                                           , path =
                                               Path
                                                   [ { feature = "root"
                                                     , index = 0
                                                     }
                                                   , { feature = "default"
                                                     , index = 1
                                                     }
                                                   , { feature = "default"
                                                     , index = 0
                                                     }
                                                   ]
                                           , properties = Dict.fromList [ ( "eventRef", PString "drawOpened" ), ( "stateRef", PString "waitingForLight" ) ]
                                           }
                                       , Node
                                           { features =
                                               { custom = Dict.fromList []
                                               , default = Nothing
                                               }
                                           , isa = Transition
                                           , name = ""
                                           , path =
                                               Path
                                                   [ { feature = "root"
                                                     , index = 0
                                                     }
                                                   , { feature = "default"
                                                     , index = 1
                                                     }
                                                   , { feature = "default"
                                                     , index = 1
                                                     }
                                                   ]
                                           , properties = Dict.fromList [ ( "eventRef", PString "lightOn" ), ( "stateRef", PString "waitingforDraw" ) ]
                                           }
                                       ]
                               }
                           , isa = State
                           , name = ""
                           , path =
                               Path
                                   [ { feature = "root"
                                     , index = 0
                                     }
                                   , { feature = "default"
                                     , index = 1
                                     }
                                   ]
                           , properties = Dict.fromList [ ( "name", PString "active" ) ]
                           }
                       , Node
                           { features =
                               { custom = Dict.fromList []
                               , default =
                                   Just
                                       [ Node
                                           { features =
                                               { custom = Dict.fromList []
                                               , default = Nothing
                                               }
                                           , isa = Transition
                                           , name = ""
                                           , path =
                                               Path
                                                   [ { feature = "root"
                                                     , index = 0
                                                     }
                                                   , { feature = "default"
                                                     , index = 2
                                                     }
                                                   , { feature = "default"
                                                     , index = 0
                                                     }
                                                   ]
                                           , properties = Dict.fromList [ ( "eventRef", PString "lightOn" ), ( "stateRef", PString "unlockedPanel" ) ]
                                           }
                                       ]
                               }
                           , isa = State
                           , name = ""
                           , path =
                               Path
                                   [ { feature = "root"
                                     , index = 0
                                     }
                                   , { feature = "default"
                                     , index = 2
                                     }
                                   ]
                           , properties = Dict.fromList [ ( "name", PString "waitingForLight" ) ]
                           }
                       , Node
                           { features =
                               { custom = Dict.fromList []
                               , default =
                                   Just
                                       [ Node
                                           { features =
                                               { custom = Dict.fromList []
                                               , default = Nothing
                                               }
                                           , isa = Transition
                                           , name = ""
                                           , path =
                                               Path
                                                   [ { feature = "root"
                                                     , index = 0
                                                     }
                                                   , { feature = "default"
                                                     , index = 3
                                                     }
                                                   , { feature = "default"
                                                     , index = 0
                                                     }
                                                   ]
                                           , properties = Dict.fromList [ ( "eventRef", PString "drawOpened" ), ( "stateRef", PString "unlockedPanel" ) ]
                                           }
                                       ]
                               }
                           , isa = State
                           , name = ""
                           , path =
                               Path
                                   [ { feature = "root"
                                     , index = 0
                                     }
                                   , { feature = "default"
                                     , index = 3
                                     }
                                   ]
                           , properties = Dict.fromList [ ( "name", PString "waitingForDraw" ) ]
                           }
                       , Node
                           { features =
                               { custom = Dict.fromList []
                               , default =
                                   Just
                                       [ Node
                                           { features =
                                               { custom = Dict.fromList []
                                               , default = Nothing
                                               }
                                           , isa = Transition
                                           , name = ""
                                           , path =
                                               Path
                                                   [ { feature = "root"
                                                     , index = 0
                                                     }
                                                   , { feature = "default"
                                                     , index = 4
                                                     }
                                                   , { feature = "default"
                                                     , index = 0
                                                     }
                                                   ]
                                           , properties = Dict.fromList [ ( "eventRef", PString "panelClosed" ), ( "stateRef", PString "idle" ) ]
                                           }
                                       ]
                               }
                           , isa = State
                           , name = ""
                           , path =
                               Path
                                   [ { feature = "root"
                                     , index = 0
                                     }
                                   , { feature = "default"
                                     , index = 4
                                     }
                                   ]
                           , properties = Dict.fromList [ ( "name", PString "unlockedPanel" ) ]
                           }
                       ]
               }
           , isa = StateMachine
           , name = "root"
           , path =
               Path
                   [ { feature = "root"
                     , index = 0
                     }
                   ]
           , properties = Dict.fromList [ ( "name", PString "H's secred compartment" ) ]
           }
-}
