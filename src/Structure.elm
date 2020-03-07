module Structure
    exposing
        ( Node
        , Property
        , propertiesOf
        , propertyStringValueOf
        , propertyIntValueOf
        , propertyBoolValueOf
        , createRoot
        , createNode
        , addProperty
        , addProperties
        , addText
        , addInt
        , addBool
        , addToDefault
        , addToText
        , addToCustom
        , addToDefaultRange
        , getUnderDefault
        , getUnderText
        , getUnderCustom
        )

import Dict exposing (..)
import Maybe exposing (..)


type Node a
    = Node NodeI


type NodeI
    = NodeI String String (Dict String Property) Features


type alias Features =
    { default : Maybe (List NodeI)
    , text : Maybe (List NodeI)
    , custom : Dict String (List NodeI)
    }


type Property
    = IntProperty Int
    | StringProperty String
    | BoolProperty Bool


propertyStringValueOf : Node a -> String -> Maybe String
propertyStringValueOf node key =
    let
        props =
            propertiesOf node
    in
        Dict.get key props
            |> Maybe.andThen
                (\prop ->
                    case prop of
                        StringProperty v ->
                            Just v

                        _ ->
                            Nothing
                )


propertyIntValueOf : Node a -> String -> Maybe Int
propertyIntValueOf node key =
    let
        props =
            propertiesOf node
    in
        Dict.get key props
            |> Maybe.andThen
                (\prop ->
                    case prop of
                        IntProperty v ->
                            Just v

                        _ ->
                            Nothing
                )


propertyBoolValueOf : Node a -> String -> Maybe Bool
propertyBoolValueOf node key =
    let
        props =
            propertiesOf node
    in
        Dict.get key props
            |> Maybe.andThen
                (\prop ->
                    case prop of
                        BoolProperty v ->
                            Just v

                        _ ->
                            Nothing
                )


propertiesOf : Node a -> Dict String Property
propertiesOf (Node (NodeI _ _ props _)) =
    props



{- featuresOf : Node a -> List Feature
   featuresOf (Node (NodeI _ _ _ feats)) =
     feats
-}


emptyFeatures : Features
emptyFeatures =
    { default = Nothing
    , text = Nothing
    , custom = Dict.empty
    }


createRoot : a -> Node a
createRoot a =
    createNode "root" a


createNode : String -> a -> Node a
createNode name _ =
    Node (NodeI name "" Dict.empty emptyFeatures)


addText : String -> String -> Node a -> Node a
addText key value node =
    addProperty key (StringProperty value) node


addInt : String -> Int -> Node a -> Node a
addInt key value node =
    addProperty key (IntProperty value) node


addBool : String -> Bool -> Node a -> Node a
addBool key value node =
    addProperty key (BoolProperty value) node


addProperty : String -> Property -> Node a -> Node a
addProperty key property (Node (NodeI name id properties features)) =
    Node (NodeI name id (Dict.insert key property properties) features)


addProperties : List Property -> Node a -> Node a
addProperties properties node =
    node



--List.foldl addProperty node properties


addToDefaultRange : List (Node a) -> Node b -> Node b
addToDefaultRange children parent =
    List.foldl addToDefault parent children


addToDefault : Node a -> Node b -> Node b
addToDefault (Node child) (Node (NodeI name idParent properties features)) =
    let
        featuresNew =
            case features.default of
                Nothing ->
                    { features | default = Just [ child ] }

                Just children ->
                    { features | default = Just (List.reverse (child :: List.reverse children)) }
    in
        Node (NodeI name idParent properties featuresNew)


addToText : Node a -> Node b -> Node b
addToText (Node child) (Node (NodeI name idParent properties features)) =
    let
        featuresNew =
            case features.text of
                Nothing ->
                    { features | text = Just ([ child ]) }

                Just children ->
                    { features | text = Just (List.reverse (child :: List.reverse children)) }
    in
        Node (NodeI name idParent properties featuresNew)


addToCustom : String -> Node a -> Node b -> Node b
addToCustom key (Node child) (Node (NodeI name idParent properties ({ custom } as features))) =
    let
        customNew =
            case Dict.get key custom of
                Nothing ->
                    Dict.insert key [ child ] custom

                Just children ->
                    Dict.update key
                        (\mbChildren ->
                            Maybe.andThen (\childrenL -> Just (List.reverse (child :: List.reverse childrenL))) mbChildren
                        )
                        custom
    in
        Node (NodeI name idParent properties { features | custom = customNew })


getUnderDefault : Node a -> Maybe (List (Node b))
getUnderDefault (Node (NodeI _ _ _ { default })) =
    default |> Maybe.andThen (\children -> Just (List.map (\ni -> Node ni) children))


getUnderText : Node a -> Maybe (List (Node b))
getUnderText (Node (NodeI _ _ _ { text })) =
    text |> Maybe.andThen (\children -> Just (List.map (\ni -> Node ni) children))


getUnderCustom : String -> Node a -> Maybe (List (Node b))
getUnderCustom key (Node (NodeI _ _ _ { custom })) =
    Dict.get key custom |> Maybe.andThen (\children -> Just (List.map (\ni -> Node ni) children))



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


   addPaths : Node a -> Node a
   addPaths (Node (NodeI name rootId properties features)) =
       let
           rootId2 = if rootId == "" then "root" else rootId
       in
           Node (NodeI name rootId2 properties (List.map (addFeatureIds rootId2) features))


   addFeatureIds : String -> Feature -> Feature
   addFeatureIds parentId feature =
       case feature of
           Main children ->
               Main <|
                   List.indexedMap (addIds (parentId ++ ":main")) children

           Text children ->
               Text <| List.indexedMap (addIds (parentId ++ ":text")) children

           Custom featureId children ->
               let
                   featureIdNew =
                       parentId ++ ":" ++ featureId
               in
                   Custom
                       featureIdNew
                   <|
                       List.indexedMap (addIds featureIdNew) children



   addIds : String -> Int -> NodeI -> NodeI
   addIds path index (NodeI name _ properties features) =
       let
           idNew =
               path ++ "_" ++ name ++ String.fromInt index
       in
           NodeI name idNew properties <|
               List.map (addFeatureIds idNew) features

-}


cell =
    Node
        (NodeI "root"
            ""
            (Dict.fromList [])
            { custom = Dict.fromList []
            , default =
                Just
                    [ NodeI "vertStackCell"
                        ""
                        (Dict.fromList [])
                        { custom = Dict.fromList []
                        , default =
                            Just
                                [ NodeI "constantCell"
                                    ""
                                    (Dict.fromList [ ( "constant", StringProperty "event" ) ])
                                    { custom = Dict.fromList []
                                    , default = Nothing
                                    , text = Nothing
                                    }
                                , NodeI "inputCell"
                                    ""
                                    (Dict.fromList [ ( "input", StringProperty "doorClosed" ) ])
                                    { custom = Dict.fromList []
                                    , default = Nothing
                                    , text = Nothing
                                    }
                                , NodeI "constantCell"
                                    ""
                                    (Dict.fromList [ ( "constant", StringProperty "end" ) ])
                                    { custom = Dict.fromList []
                                    , default = Nothing
                                    , text = Nothing
                                    }
                                ]
                        , text = Nothing
                        }
                    ]
            , text = Nothing
            }
        )
