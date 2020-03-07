module Structure
    exposing
        ( Node
        , Property
        , isaOf
        , addPaths
        , propertiesOf
        , propertyStringValueOf
        , propertyIntValueOf
        , propertyBoolValueOf
        , createRoot
        , createNode
        , addText, addInt, addBool
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
isaOf (Node {isa}) =
  isa

propertyValueOf : Node a -> String -> Maybe Primitive
propertyValueOf (Node { properties }) key =
    Dict.get key properties


propertyStringValueOf : Node a -> String -> Maybe String
propertyStringValueOf node key =
    propertyValueOf node key
        |> Maybe.andThen
            (\prop ->
                case prop of
                    PString v ->
                        Just v

                    _ ->
                        Nothing
            )


propertyIntValueOf : Node a -> String -> Maybe Int
propertyIntValueOf node key =
    propertyValueOf node key
        |> Maybe.andThen
            (\prop ->
                case prop of
                    PInt v ->
                        Just v

                    _ ->
                        Nothing
            )


propertyBoolValueOf : Node a -> String -> Maybe Bool
propertyBoolValueOf node key =
    propertyValueOf node key
        |> Maybe.andThen
            (\prop ->
                case prop of
                    PBool v ->
                        Just v

                    _ ->
                        Nothing
            )


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
  addProperty (key, PString text) node

addInt : String -> Int -> Node a -> Node a
addInt key value node =
  addProperty (key, PInt value) node

addBool : String -> Bool -> Node a -> Node a
addBool key value node =
  addProperty (key, PBool value) node


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
            case Dict.get key features.custom of
                Nothing ->
                    Dict.insert key [ child ] features.custom

                Just children ->
                    Dict.update key
                        (\mbChildren ->
                            Maybe.andThen (\childrenL -> Just (List.reverse (child :: List.reverse childrenL))) mbChildren
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
addPaths (Node ({path} as data)) =
       Node { data | features = addFeaturePath path data.features }


addFeaturePath : String -> Features a -> Features a
addFeaturePath parentId { default, custom } =
    let
        indexUpdater postFix =
            List.indexedMap (addPath (parentId ++ postFix))

        defaultNew = 
            Maybe.map (\children -> indexUpdater ":default" children) default

        customNew =
            Dict.map (\k children -> indexUpdater (parentId ++ ":" ++ k) children) custom
    in
        { default = defaultNew
        , custom = customNew 
        }


addPath : String -> Int -> Node a -> Node a
addPath pathParent index (Node ({path} as data)) =
    let
        pathNew =
            pathParent ++ String.fromInt index
    in
        Node { data | path = pathNew, features = addFeaturePath path data.features }

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
    ""