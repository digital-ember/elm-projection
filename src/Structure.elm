module Structure
    exposing
        ( Node
        , Property
        , isaOf
        , pathOf
        , addPaths
        , propertiesOf
        , textOf
        , intOf
        , boolOf
        , createRoot
        , createNode
        , addText
        , addInt
        , addBool
        , addProperty
        , addProperties
        , addToDefault
        , addToCustom
        , addToDefaultRange
        , getUnderDefault
        , getUnderCustom
        )

import Dict exposing (..)
import Maybe exposing (..)


type Node a
    = Node
        { id : String
        , path : String
        , isa : a
        , properties : Dict String Primitive
        , features : Features a
        }


type alias Features a =
    { default : Maybe (List (Node a))
    , custom : Dict String (List (Node a))
    }


type alias Property =
    ( String, Primitive )


type Primitive
    = PInt Int
    | PString String
    | PBool Bool


isaOf : Node a -> a
isaOf (Node { isa }) =
    isa


pathOf : Node a -> String
pathOf (Node { path }) =
    path


valueOf : String -> Node a -> Maybe Primitive
valueOf key (Node { properties }) =
    Dict.get key properties


textOf : String -> Node a -> String
textOf key node =
    valueOf key node
        |> Maybe.andThen
            (\prop ->
                case prop of
                    PString v ->
                        Just v

                    _ ->
                        Nothing
            )
        |> Maybe.withDefault ""


intOf : String -> Node a -> Int
intOf key node =
    valueOf key node
        |> Maybe.andThen
            (\prop ->
                case prop of
                    PInt v ->
                        Just v

                    _ ->
                        Nothing
            )
        |> Maybe.withDefault 0


boolOf : String -> Node a -> Bool
boolOf key node =
    valueOf key node 
        |> Maybe.andThen
            (\prop ->
                case prop of
                    PBool v ->
                        Just v

                    _ ->
                        Nothing
            )
        |> Maybe.withDefault False


propertiesOf : Node a -> Dict String Primitive
propertiesOf (Node { properties }) =
    properties



{- featuresOf : Node a -> List Feature
   featuresOf (Node (NodeI _ _ _ feats)) =
     feats
-}


emptyFeatures : Features a
emptyFeatures =
    { default = Nothing
    , custom = Dict.empty
    }


createRoot : a -> Node a
createRoot isa =
    createNodeInternal "root" isa


createNode : a -> Node a
createNode isa =
    createNodeInternal "" isa


createNodeInternal : String -> a -> Node a
createNodeInternal id isa =
    Node
        { id = id
        , isa = isa
        , path = id
        , properties = Dict.empty
        , features = emptyFeatures
        }


addText : String -> String -> Node a -> Node a
addText key text node =
    addProperty ( key, PString text ) node


addInt : String -> Int -> Node a -> Node a
addInt key value node =
    addProperty ( key, PInt value ) node


addBool : String -> Bool -> Node a -> Node a
addBool key value node =
    addProperty ( key, PBool value ) node


addProperty : Property -> Node a -> Node a
addProperty ( key, value ) (Node data) =
    Node { data | properties = Dict.insert key value data.properties }


addProperties : List Property -> Node a -> Node a
addProperties properties node =
    List.foldl addProperty node properties


addToDefaultRange : List (Node a) -> Node a -> Node a
addToDefaultRange children parent =
    List.foldl addToDefault parent children


addToDefault : Node a -> Node a -> Node a
addToDefault child (Node ({ features } as data)) =
    let
        featuresNew =
            case features.default of
                Nothing ->
                    { features | default = Just [ child ] }

                Just children ->
                    { features | default = Just (List.reverse (child :: List.reverse children)) }
    in
        Node { data | features = featuresNew }


addToCustom : String -> Node a -> Node a -> Node a
addToCustom key child (Node ({ features } as data)) =
    let
        customNew =
            Dict.update key
                (\mbChildren ->
                    Just <|
                        case mbChildren of
                            Nothing ->
                                [ child ]
                        
                            Just children ->
                                List.reverse (child :: List.reverse children)
                )
                features.custom
                    

        featuresNew =
            { features | custom = customNew }
    in
        Node { data | features = featuresNew }


getUnderDefault : Node a -> Maybe (List (Node a))
getUnderDefault (Node { features }) =
    features.default


getUnderCustom : String -> Node a -> Maybe (List (Node a))
getUnderCustom key (Node { features }) =
    Dict.get key features.custom


addPaths : Node a -> Node a
addPaths (Node ({ path } as data)) =
    Node { data | features = addFeaturePath path data.features }


addFeaturePath : String -> Features a -> Features a
addFeaturePath pathParent { default, custom } =
    let
        indexUpdater postFix =
            List.indexedMap (addPath (pathParent ++ postFix))

        defaultNew =
            Maybe.map (\children -> indexUpdater ":default" children) default

        customNew =
            Dict.map (\k children -> indexUpdater (":" ++ k) children) custom
    in
        { default = defaultNew
        , custom = customNew
        }


addPath : String -> Int -> Node a -> Node a
addPath pathParent index (Node ({ path } as data)) =
    let
        pathNew =
            pathParent ++ String.fromInt index
    in
        Node { data | path = pathNew, features = addFeaturePath pathNew data.features }



{-
   addFeatures : List Feature -> Node a -> Node a
   addFeatures features parent =
     List.foldl addFeature parent features

   addFeature : Feature -> Node a -> Node a
   addFeature feature parent =
     case feature of
         Main children ->
           List.foldl addToMain parent
             <| List.map (\ni -> Node ni) children

         Text children ->
           List.foldl addToText parent
             <| List.map (\ni -> Node ni) children

         Custom id children ->
           List.foldl (addToCustom id) parent
             <| List.map (\ni -> Node ni) children



-}


cell =
    ""{-
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
                                        , id = ""
                                        , isa = ConstantCell
                                        , path = "root:default0:default0"
                                        , properties = Dict.fromList [ ( "constant", PString "event" ) ]
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
                                                        , id = ""
                                                        , isa = InputCell
                                                        , path = "root:default0:default1:default0"
                                                        , properties = Dict.fromList [ ( "input", PString "doorClosed" ) ]
                                                        }
                                                    ]
                                            }
                                        , id = ""
                                        , isa = StackCell Vert
                                        , path = "root:default0:default1"
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
                                                        , id = ""
                                                        , isa = InputCell
                                                        , path = "root:default0:default2:default0"
                                                        , properties = Dict.fromList [ ( "input", PString "doorOpened" ) ]
                                                        }
                                                    ]
                                            }
                                        , id = ""
                                        , isa = StackCell Vert
                                        , path = "root:default0:default2"
                                        , properties = Dict.fromList []
                                        }
                                    , Node
                                        { features =
                                            { custom = Dict.fromList []
                                            , default = Nothing
                                            }
                                        , id = ""
                                        , isa = ConstantCell
                                        , path = "root:default0:default3"
                                        , properties = Dict.fromList [ ( "constant", PString "end" ) ]
                                        }
                                    ]
                            }
                        , id = ""
                        , isa = StackCell Vert
                        , path = "root:default0"
                        , properties = Dict.fromList []
                        }
                    ]
            }
        , id = "root"
        , isa = RootCell
        , path = "root"
        , properties = Dict.fromList []
        }
-}