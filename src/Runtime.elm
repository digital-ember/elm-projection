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
                                               , default = Nothing
                                               }
                                           , isa = Transition
                                           , name = ""
                                           , path =
                                               Path
                                                   [ { feature = ""
                                                     , index = 0
                                                     }
                                                   ]
                                           , properties = Dict.fromList [ ( "eventRef", PString "" ), ( "stateRef", PString "" ) ]
                                           }
                                       ]
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
                           , properties = Dict.fromList [ ( "eventRef", PString "dödel" ), ( "stateRef", PString "" ) ]
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
           , properties = Dict.fromList [ ( "name", PString "" ) ]
           }




model2 =
    Node
        { features =
            { custom =
                Dict.fromList
                    [ ( "effects"
                      , [ Node
                            { features =
                                { custom = Dict.fromList []
                                , default = Nothing
                                }
                            , isa = 
                                EffectCell
                                    (InsertionEffect
                                        { feature = ""
                                        , isReplace = False
                                        , nodeToInsert =
                                            Node
                                                { features =
                                                    { custom = Dict.fromList []
                                                    , default = Nothing
                                                    }
                                                , isa = Event
                                                , name = ""
                                                , path =
                                                    Path
                                                        [ { feature = ""
                                                          , index = 0
                                                          }
                                                        ]
                                                , properties = Dict.fromList [ ( "name", PString "" ) ]
                                                }
                                        , path =
                                            Path
                                                [ { feature = "root"
                                                  , index = 0
                                                  }
                                                , { feature = "events"
                                                  , index = 1
                                                  }
                                                ]
                                        }
                                    )
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
                                      , index = 2
                                      }
                                    , { feature = "default"
                                      , index = 1
                                      }
                                    , { feature = "default"
                                      , index = 1
                                      }
                                    , { feature = "effects"
                                      , index = 0
                                      }
                                    ]
                            , properties = Dict.fromList []
                            }
                        , Node
                            { features =
                                { custom = Dict.fromList []
                                , default = Nothing
                                }
                            , isa =
                                EffectCell
                                    (OnDeleteEffect
                                        { effectHandler = ""
                                        , effectInput =
                                            Node
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
                                                , properties = Dict.fromList [ ( "name", PString "def" ) ]
                                                }
                                        , selection =
                                            { dir = ""
                                            , end = -1
                                            , start = -1
                                            }
                                        }
                                    )
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
                                      , index = 2
                                      }
                                    , { feature = "default"
                                      , index = 1
                                      }
                                    , { feature = "default"
                                      , index = 1
                                      }
                                    , { feature = "effects"
                                      , index = 1
                                      }
                                    ]
                            , properties = Dict.fromList []
                            }
                        , Node
                            { features =
                                { custom = Dict.fromList []
                                , default = Nothing
                                }
                            , isa =
                                EffectCell
                                    (OnInputEffect
                                        { key = "name"
                                        , path =
                                            Path
                                                [ { feature = "root"
                                                  , index = 0
                                                  }
                                                , { feature = "events"
                                                  , index = 1
                                                  }
                                                ]
                                        }
                                    )
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
                                      , index = 2
                                      }
                                    , { feature = "default"
                                      , index = 1
                                      }
                                    , { feature = "default"
                                      , index = 1
                                      }
                                    , { feature = "effects"
                                      , index = 2
                                      }
                                    ]
                            , properties = Dict.fromList []
                            }
                        ]
                      )
                    ]
            , default = Nothing
            }
        , isa = ContentCell InputCell
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
                  , index = 2
                  }
                , { feature = "default"
                  , index = 1
                  }
                , { feature = "default"
                  , index = 1
                  }
                ]
        , properties = Dict.fromList [ ( "input", PString "def" ) ]
        }




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
                                                               { custom = Dict.fromList []
                                                               , default = Nothing
                                                               }
                                                           , isa = ContentCell ConstantCell
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
                                                                   , { feature = "default"
                                                                     , index = 0
                                                                     }
                                                                   ]
                                                           , properties = Dict.fromList [ ( "constant", PString "name:" ) ]
                                                           }
                                                       , Node
                                                           { features =
                                                               { custom =
                                                                   Dict.fromList
                                                                       [ ( "effects"
                                                                         , [ Node
                                                                               { features =
                                                                                   { custom = Dict.fromList []
                                                                                   , default = Nothing
                                                                                   }
                                                                               , isa =
                                                                                   EffectCell
                                                                                       (OnInputEffect
                                                                                           { effectHandler = "<function>"
                                                                                           , effectInput =
                                                                                               ( Node
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
                                                                                                                   ]
                                                                                                                 )
                                                                                                               ]
                                                                                                       , default = Nothing
                                                                                                       }
                                                                                                   , isa = StateMachine
                                                                                                   , name = "root"
                                                                                                   , path =
                                                                                                       Path
                                                                                                           [ { feature = "root"
                                                                                                             , index = 0
                                                                                                             }
                                                                                                           ]
                                                                                                   , properties = Dict.fromList []
                                                                                                   }
                                                                                               , Path
                                                                                                   [ { feature = "root"
                                                                                                     , index = 0
                                                                                                     }
                                                                                                   ]
                                                                                               , "name"
                                                                                               )
                                                                                           }
                                                                                       )
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
                                                                                       , { feature = "default"
                                                                                         , index = 1
                                                                                         }
                                                                                       , { feature = "effects"
                                                                                         , index = 0
                                                                                         }
                                                                                       ]
                                                                               , properties = Dict.fromList []
                                                                               }
                                                                           ]
                                                                         )
                                                                       ]
                                                               , default = Nothing
                                                               }
                                                           , isa = ContentCell InputCell
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
                                                                   , { feature = "default"
                                                                     , index = 1
                                                                     }
                                                                   ]
                                                           , properties = Dict.fromList [ ( "input", PString "" ) ]
                                                           }
                                                       ]
                                               }
                                           , isa = ContentCell (StackCell Horiz)
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
                                           , properties = Dict.fromList []
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
                                                           , isa = ContentCell ConstantCell
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
                                                                     , index = 1
                                                                     }
                                                                   , { feature = "default"
                                                                     , index = 0
                                                                     }
                                                                   ]
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
                                                                                                   { custom = Dict.fromList []
                                                                                                   , default = Nothing
                                                                                                   }
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
                                                                                                                                   ]
                                                                                                                                 )
                                                                                                                               ]
                                                                                                                       , default = Nothing
                                                                                                                       }
                                                                                                                   , isa = StateMachine
                                                                                                                   , name = "root"
                                                                                                                   , path =
                                                                                                                       Path
                                                                                                                           [ { feature = "root"
                                                                                                                             , index = 0
                                                                                                                             }
                                                                                                                           ]
                                                                                                                   , properties = Dict.fromList []
                                                                                                                   }
                                                                                                           }
                                                                                                       )
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
                                                                                                         , index = 1
                                                                                                         }
                                                                                                       , { feature = "default"
                                                                                                         , index = 1
                                                                                                         }
                                                                                                       , { feature = "default"
                                                                                                         , index = 0
                                                                                                         }
                                                                                                       , { feature = "effects"
                                                                                                         , index = 0
                                                                                                         }
                                                                                                       ]
                                                                                               , properties = Dict.fromList []
                                                                                               }
                                                                                           , Node
                                                                                               { features =
                                                                                                   { custom = Dict.fromList []
                                                                                                   , default = Nothing
                                                                                                   }
                                                                                               , isa =
                                                                                                   EffectCell
                                                                                                       (OnInputEffect
                                                                                                           { effectHandler = "<function>"
                                                                                                           , effectInput =
                                                                                                               ( Node
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
                                                                                                                                   ]
                                                                                                                                 )
                                                                                                                               ]
                                                                                                                       , default = Nothing
                                                                                                                       }
                                                                                                                   , isa = StateMachine
                                                                                                                   , name = "root"
                                                                                                                   , path =
                                                                                                                       Path
                                                                                                                           [ { feature = "root"
                                                                                                                             , index = 0
                                                                                                                             }
                                                                                                                           ]
                                                                                                                   , properties = Dict.fromList []
                                                                                                                   }
                                                                                                               , Path
                                                                                                                   [ { feature = "root"
                                                                                                                     , index = 0
                                                                                                                     }
                                                                                                                   , { feature = "events"
                                                                                                                     , index = 0
                                                                                                                     }
                                                                                                                   ]
                                                                                                               , "name"
                                                                                                               )
                                                                                                           }
                                                                                                       )
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
                                                                                                         , index = 1
                                                                                                         }
                                                                                                       , { feature = "default"
                                                                                                         , index = 1
                                                                                                         }
                                                                                                       , { feature = "default"
                                                                                                         , index = 0
                                                                                                         }
                                                                                                       , { feature = "effects"
                                                                                                         , index = 1
                                                                                                         }
                                                                                                       ]
                                                                                               , properties = Dict.fromList []
                                                                                               }
                                                                                           ]
                                                                                         )
                                                                                       ]
                                                                               , default = Nothing
                                                                               }
                                                                           , isa = ContentCell InputCell
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
                                                                                     , index = 1
                                                                                     }
                                                                                   , { feature = "default"
                                                                                     , index = 1
                                                                                     }
                                                                                   , { feature = "default"
                                                                                     , index = 0
                                                                                     }
                                                                                   ]
                                                                           , properties = Dict.fromList [ ( "input", PString "doorClosed" ) ]
                                                                           }
                                                                       ]
                                                               }
                                                           , isa = ContentCell (StackCell Vert)
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
                                                                     , index = 1
                                                                     }
                                                                   , { feature = "default"
                                                                     , index = 1
                                                                     }
                                                                   ]
                                                           , properties = Dict.fromList [ ( "indent", PBool True ) ]
                                                           }
                                                       , Node
                                                           { features =
                                                               { custom = Dict.fromList []
                                                               , default = Nothing
                                                               }
                                                           , isa = ContentCell ConstantCell
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
                                                                     , index = 1
                                                                     }
                                                                   , { feature = "default"
                                                                     , index = 2
                                                                     }
                                                                   ]
                                                           , properties = Dict.fromList [ ( "constant", PString "end" ) ]
                                                           }
                                                       ]
                                               }
                                           , isa = ContentCell (StackCell Vert)
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
                                                     , index = 1
                                                     }
                                                   ]
                                           , properties = Dict.fromList []
                                           }
                                       ]
                               }
                           , isa = ContentCell (StackCell Vert)
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
                           , properties = Dict.fromList []
                           }
                       ]
               }
           , isa = ContentCell RootCell
           , name = "root"
           , path =
               Path
                   [ { feature = "root"
                     , index = 0
                     }
                   ]
           , properties = Dict.fromList []
           }
-}
