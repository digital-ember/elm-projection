module Runtime exposing (program, Msg, Model)

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
    | EditorMsg (Node (Cell a)) (Editor.Msg a)


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

        EditorMsg cellModel eMsg ->
            let
                ( domainModelNew, editorCmd ) = 
                    updateEditor eMsg cellModel model.domainModel
            in
                ( { model | domainModel = domainModelNew }, Cmd.map (\editorMsg -> EditorMsg cellModel editorMsg) editorCmd )


view : Model a -> Html (Msg a)
view model =
    let

        cellModel =
            model.xform model.domainModel |> updatePaths

        --|> Debug.log "cell"
    in
        Html.map (\editorMsg -> EditorMsg cellModel editorMsg) (viewEditor cellModel)

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