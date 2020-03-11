module Structure
    exposing
        ( Node
        , Property
        , Path(..)
        , isaOf
        , pathOf
        , pathAsId
        , updatePaths
        , propertiesOf
        , stringProperty
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
        , insertAfterUnderDefault
        , insertAfterUnderCustom
        , updatePropertyByPath
        , getUnderDefault
        , getUnderCustom
        )

import Dict exposing (..)
import Maybe exposing (..)


type Node a
    = Node
        { name : String
        , path : Path
        , isa : a
        , properties : Dict String Primitive
        , features : Features a
        }


type Path
    = Path (List PathSegment)


type alias PathSegment =
    { feature : String
    , index : Int
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


pathOf : Node a -> Path
pathOf (Node { path }) =
    path

pathAsId : Node a -> String
pathAsId (Node {path}) = 
    let
       (Path segments) = path
    in
       List.foldl pathSegmentAsId "" segments
  
pathSegmentAsId : PathSegment -> String -> String
pathSegmentAsId {feature, index} idPart =
    idPart ++ (if idPart=="" then "" else "-") ++ feature ++ String.fromInt index

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


stringProperty : ( String, String ) -> Property
stringProperty ( k, v ) =
    ( k, PString v )



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
createNodeInternal name isa =
    Node
        { name = name
        , isa = isa
        , path = Path [ { feature = name, index = 0 } ]
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


insertAfterUnderDefault : Node a -> Path -> Node a -> Node a
insertAfterUnderDefault child pathAfter (Node ({ features } as data)) =
    let
        featuresNew =
            case features.default of
                Nothing ->
                    { features | default = Just [ child ] }

                Just children ->
                    { features | default = Just (List.foldl (insertAfter pathAfter child) [] children) }
    in
        Node { data | features = featuresNew }


insertAfterUnderCustom : String -> Node a -> Path -> Node a -> Node a
insertAfterUnderCustom key child pathAfter (Node ({ features } as data)) =
    let
        customNew =
            Dict.update key
                (\mbChildren ->
                    Just <|
                        case mbChildren of
                            Nothing ->
                                [ child ]

                            Just children ->
                                List.foldl (insertAfter pathAfter child) [] children
                )
                features.custom

        featuresNew =
            { features | custom = customNew }
    in
        Node { data | features = featuresNew }


insertAfter : Path -> Node a -> Node a -> List (Node a) -> List (Node a)
insertAfter pathAfter child candidate result =
    if pathAfter == pathOf candidate then
        result ++ [ candidate, child ]
    else
        result ++ [ candidate ]


getUnderDefault : Node a -> Maybe (List (Node a))
getUnderDefault (Node { features }) =
    features.default


getUnderCustom : String -> Node a -> Maybe (List (Node a))
getUnderCustom key (Node { features }) =
    Dict.get key features.custom


updatePropertyByPath : Node a -> Path -> Property -> Node a
updatePropertyByPath root (Path segments) property =
    updatePropertyRec root segments property


updatePropertyRec : Node a -> List PathSegment -> Property -> Node a
updatePropertyRec parent segments property =
    case segments of
        -- reached end of path => update property for that node!
        [] ->
            updateProperty parent property

        ({ feature } as segment) :: tail ->
            case feature of
                -- special handling for root node which has no parent => we skip its segment
                "root" ->
                    updatePropertyRec parent tail property

                
                _ ->
                    updateChildrenUnder parent segment tail property


updateChildrenUnder : Node a -> PathSegment -> List PathSegment -> Property -> Node a
updateChildrenUnder parent ({ feature } as segment) tailSegments property =
    if feature == "default" then
        updateChildrenUnderDefault parent segment tailSegments property
    else
        updateChildrenUnderCustom parent segment tailSegments property



updateChildrenUnderDefault : Node a -> PathSegment -> List PathSegment -> Property -> Node a
updateChildrenUnderDefault ((Node ({ features } as data)) as parent) { index } tailSegments property =
  let
        mbChildrenNew =
            getUnderDefault parent
                |> Maybe.andThen (updateChildren index tailSegments property)

        featuresNew =
            { features | default = mbChildrenNew }
    in
        Node { data | features = featuresNew }

updateChildrenUnderCustom : Node a -> PathSegment -> List PathSegment -> Property -> Node a
updateChildrenUnderCustom ((Node ({ features } as data)) as parent) { feature, index } tailSegments property =
  let
        mbChildrenNew =
            getUnderCustom feature parent
                |> Maybe.andThen (updateChildren index tailSegments property)

        featuresNew =
            { features | custom = Dict.update feature (\_ -> mbChildrenNew) features.custom }
    in
        Node { data | features = featuresNew }

updateChildren : Int -> List PathSegment -> Property -> List (Node a) -> Maybe (List (Node a))
updateChildren index tailSegments property children =
    let
        updateAt i child =
            if i == index then
                updatePropertyRec child tailSegments property
            else
                child
    in
        Just <| 
            List.indexedMap updateAt children


updateProperty : Node a -> Property -> Node a
updateProperty (Node data) (key, primitiveNew) =
    Node { data | properties = Dict.update key (\_ -> Just primitiveNew) data.properties }


updatePaths : Node a -> Node a
updatePaths (Node ({ path } as data)) =
    Node { data | features = addFeaturePath path data.features }


addFeaturePath : Path -> Features a -> Features a
addFeaturePath parentPath { default, custom } =
    let
        indexUpdater feature =
            List.indexedMap (addPath parentPath feature)

        defaultNew =
            Maybe.map (\children -> indexUpdater "default" children) default

        customNew =
            Dict.map (\key children -> indexUpdater key children) custom
    in
        { default = defaultNew
        , custom = customNew
        }


addPath : Path -> String -> Int -> Node a -> Node a
addPath (Path parentSegments) feature index (Node data) =
    let
        pathNew =
            Path <|
                List.reverse <|
                    { feature = feature, index = index }
                        :: List.reverse parentSegments
    in
        Node { data | path = pathNew, features = addFeaturePath pathNew data.features }
