module Structure
    exposing
        ( Node
        , Property
        , Path(..)
        , PathSegment
        , isaOf
        , pathOf
        , lengthOf
        , pathAsId
        , pathAsIdFromNode
        , splitLastPathSegment
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
        , addChildAtPathToDefault
        , addChildAtPathToCustom
        , insertAfterUnderDefault
        , insertAfterUnderCustom
        , insertChildAfterPath
        , replaceUnderDefault
        , replaceUnderCustom
        , deleteNode
        , updatePropertyByPath
        , getUnderDefault
        , getUnderCustom
        , parentOf
        , previousSibling
        , nextSibling
        )

import Dict exposing (..)
import Maybe exposing (..)
import Array exposing (..)


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
    { default : List (Node a)
    , custom : Dict String (List (Node a))
    }


type alias Property =
    ( String, Primitive )


type Primitive
    = PInt Int
    | PString String
    | PBool Bool



strDefault : String
strDefault =
    "default"


isaOf : Node a -> a
isaOf (Node { isa }) =
    isa


pathOf : Node a -> Path
pathOf (Node { path }) =
    path


lengthOf : Path -> Int
lengthOf (Path segments) =
    List.length segments


pathAsIdFromNode : Node a -> String
pathAsIdFromNode (Node { path }) =
    pathAsId path


pathAsId : Path -> String
pathAsId path =
    let
        (Path segments) =
            path
    in
        List.foldl pathSegmentAsId "" segments


pathSegmentAsId : PathSegment -> String -> String
pathSegmentAsId { feature, index } idPart =
    idPart
        ++ (if idPart == "" then
                ""
            else
                "-"
           )
        ++ feature
        ++ String.fromInt index


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


emptyFeatures : Features a
emptyFeatures =
    { default = []
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
                [] ->
                    { features | default = [ child ] }

                children ->
                    { features | default = List.reverse (child :: List.reverse children) }
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


replaceUnderDefault : List (Node a) -> Node a -> Node a
replaceUnderDefault children (Node ({ features } as data)) =
    let
        featuresNew =
            { features | default = children }
    in
        Node { data | features = featuresNew }


replaceUnderCustom : String -> List (Node a) -> Node a -> Node a
replaceUnderCustom key children (Node ({ features } as data)) =
    let
        featuresNew =
            { features | custom = Dict.insert key children features.custom }
    in
        Node { data | features = featuresNew }


insertAfterUnderDefault : Node a -> Path -> Node a -> Node a
insertAfterUnderDefault child pathAfter (Node ({ features } as data)) =
    let
        featuresNew =
            case features.default of
                [] ->
                    { features | default = [ child ] }

                children ->
                    { features | default = List.foldl (insertAfter pathAfter child) [] children }
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


getUnder : String -> Node a -> List (Node a)
getUnder feature node =
    if feature == strDefault then
        getUnderDefault node
    else
        getUnderCustom feature node


getUnderDefault : Node a -> List (Node a)
getUnderDefault (Node { features }) =
    features.default


getUnderCustom : String -> Node a -> List (Node a)
getUnderCustom key (Node { features }) =
    Dict.get key features.custom
        |> Maybe.withDefault []


addChildAtPathToDefault : Node a -> Path -> Node a -> Node a
addChildAtPathToDefault nodeNew path root =
    let
        (Path segmentsNoRoot) =
            dropRootSegment path
    in
        addChildAtPathToDefaultRec nodeNew segmentsNoRoot root


addChildAtPathToDefaultRec : Node a -> List PathSegment -> Node a -> Node a
addChildAtPathToDefaultRec nodeNew segments parent =
    case segments of
        [] ->
            addToDefault nodeNew parent

        segment :: tail ->
            addChildrenDefault nodeNew segment tail parent


addChildrenDefault : Node a -> PathSegment -> List PathSegment -> Node a -> Node a
addChildrenDefault nodeNew { feature, index } tailSegments ((Node ({ features } as data)) as parent) =
    let
        childrenNew =
            (if feature == strDefault then
                getUnderDefault parent
            else
                getUnderCustom feature parent
            )
            |> getNewChildrenForRecDefaultAdd nodeNew index tailSegments

        featuresNew =
            { features | default = childrenNew }
    in
        Node { data | features = featuresNew }


getNewChildrenForRecDefaultAdd : Node a -> Int -> List PathSegment -> List (Node a) -> List (Node a)
getNewChildrenForRecDefaultAdd nodeNew index tailSegments children =
    let
        insertAt i child =
            if i == index then
                addChildAtPathToDefaultRec nodeNew tailSegments child
            else
                child
    in
        List.indexedMap insertAt children


addChildAtPathToCustom : String -> Node a -> Path -> Node a -> Node a
addChildAtPathToCustom key nodeNew path root =
    let
        (Path segmentsNoRoot) =
            dropRootSegment path
    in
        addChildAtPathToCustomRec key nodeNew segmentsNoRoot root


addChildAtPathToCustomRec : String -> Node a -> List PathSegment -> Node a -> Node a
addChildAtPathToCustomRec key nodeNew segments parent =
    case segments of
        [] ->
            addToCustom key nodeNew parent

        segment :: tail ->
            addChildrenCustom key nodeNew segment tail parent


addChildrenCustom : String -> Node a -> PathSegment -> List PathSegment -> Node a -> Node a
addChildrenCustom key nodeNew { feature, index } tailSegments ((Node ({ features } as data)) as parent) =
    let
        childrenNew =
            (if feature == strDefault then
                getUnderDefault parent
            else
                getUnderCustom feature parent
            )
            |> getNewChildrenForRecCustomAdd key nodeNew index tailSegments

        featuresNew =
            { features | default = childrenNew }
    in
        Node { data | features = featuresNew }


getNewChildrenForRecCustomAdd : String -> Node a -> Int -> List PathSegment -> List (Node a) -> List (Node a)
getNewChildrenForRecCustomAdd key nodeNew index tailSegments children =
    let
        insertAt i child =
            if i == index then
                addChildAtPathToCustomRec key nodeNew tailSegments child
            else
                child
    in
        List.indexedMap insertAt children


insertChildAfterPath : Node a -> Path -> Node a -> Node a
insertChildAfterPath nodeNew path root =
    let
        (Path segmentsNoRoot) =
            dropRootSegment path
    in
        insertChildAfterPathRec nodeNew path segmentsNoRoot root


insertChildAfterPathRec : Node a -> Path -> List PathSegment -> Node a -> Node a
insertChildAfterPathRec nodeNew pathAfter segments parent =
    case segments of
        segment :: [] ->
            insertChildAfter nodeNew pathAfter segment parent

        segment :: tail ->
            insertChildrenUnder nodeNew pathAfter segment tail parent

        _ ->
            parent


insertChildrenUnder : Node a -> Path -> PathSegment -> List PathSegment -> Node a -> Node a
insertChildrenUnder nodeNew pathAfter ({ feature } as segment) tailSegments parent =
    if feature == strDefault then
        insertChildrenUnderDefault nodeNew pathAfter segment tailSegments parent
    else
        insertChildrenUnderCustom nodeNew pathAfter segment tailSegments parent


insertChildrenUnderDefault : Node a -> Path -> PathSegment -> List PathSegment -> Node a -> Node a
insertChildrenUnderDefault nodeNew pathAfter segment tailSegments ((Node ({ features } as data)) as parent) =
    let
        childrenNew =
            getUnderDefault parent
                |> getNewChildrenForRecInsert nodeNew pathAfter segment tailSegments

        featuresNew =
            { features | default = childrenNew }
    in
        Node { data | features = featuresNew }


insertChildrenUnderCustom : Node a -> Path -> PathSegment -> List PathSegment -> Node a -> Node a
insertChildrenUnderCustom nodeNew pathAfter segment tailSegments ((Node ({ features } as data)) as parent) =
    let
        childrenNew =
            getUnderCustom segment.feature parent
                |> getNewChildrenForRecInsert nodeNew pathAfter segment tailSegments

        featuresNew =
            { features | custom = Dict.insert segment.feature childrenNew features.custom }
    in
        Node { data | features = featuresNew }


getNewChildrenForRecInsert : Node a -> Path -> PathSegment -> List PathSegment -> List (Node a) -> List (Node a)
getNewChildrenForRecInsert nodeNew pathAfter segment tailSegments children =
    let
        insertAt i child =
            if i == segment.index then
                insertChildAfterPathRec nodeNew pathAfter tailSegments child
            else
                child
    in
        List.indexedMap insertAt children


insertChildAfter : Node a -> Path -> PathSegment -> Node a -> Node a
insertChildAfter nodeNew pathAfter { feature } parent =
    if feature == strDefault then
        insertAfterUnderDefault nodeNew pathAfter parent
    else
        insertAfterUnderCustom feature nodeNew pathAfter parent


deleteNode : Path -> Node a -> Node a
deleteNode path root =
    let
        (Path segmentsNoRoot) =
            dropRootSegment path
    in
        deleteNodeRec segmentsNoRoot root 


deleteNodeRec : List PathSegment -> Node a -> Node a
deleteNodeRec segments parent =
    case segments of
        ({feature, index} as segment) :: [] ->
            if feature == strDefault then 
                deleteNodeUnderDefault index parent
            else
                deleteNodeUnderCustom segment parent

        segment :: tail ->
            deleteNodeNested segment tail parent

        [] ->
            parent


deleteNodeNested : PathSegment -> List PathSegment -> Node a -> Node a
deleteNodeNested { feature, index } tailSegments ((Node ({ features } as data)) as parent) =
    let
        childrenNew =
            (if feature == strDefault then
                getUnderDefault parent
            else
                getUnderCustom feature parent
            )
            |> getNewChildrenForRecDelete index tailSegments

        featuresNew =
            { features | default = childrenNew }
    in
        Node { data | features = featuresNew }


getNewChildrenForRecDelete : Int -> List PathSegment -> List (Node a) -> List (Node a)
getNewChildrenForRecDelete index tailSegments children =
    let
        deleteRec i child =
            if i == index then
                deleteNodeRec tailSegments child
            else
                child
    in
        List.indexedMap deleteRec children

deleteNodeUnderDefault : Int -> Node a -> Node a
deleteNodeUnderDefault index parent = 
    let
        delete children =
            List.indexedMap (\i c -> if i == index  then Nothing else Just c) children
            |> List.filterMap identity


        childrenNew =
            getUnderDefault parent
                |> delete
    in
        replaceUnderDefault childrenNew parent

deleteNodeUnderCustom : PathSegment -> Node a -> Node a
deleteNodeUnderCustom { feature, index } parent = 
    let
        delete children =
            List.indexedMap (\i c -> if i == index  then Nothing else Just c) children
            |> List.filterMap identity


        childrenNew =
            getUnderCustom feature parent
                |> delete
    in
        replaceUnderCustom feature childrenNew parent


updatePropertyByPath : Node a -> Path -> ( String, String ) -> Node a
updatePropertyByPath root (Path segments) kvp =
    updatePropertyRec root segments kvp


updatePropertyRec : Node a -> List PathSegment -> ( String, String ) -> Node a
updatePropertyRec parent segments kvp =
    case segments of
        -- reached end of path => update value for that node!
        [] ->
            updateProperty parent kvp

        ({ feature } as segment) :: tail ->
            case feature of
                -- special handling for root node which has no parent => we skip its segment
                "root" ->
                    updatePropertyRec parent tail kvp

                _ ->
                    updateChildrenUnder parent segment tail kvp


updateChildrenUnder : Node a -> PathSegment -> List PathSegment -> ( String, String ) -> Node a
updateChildrenUnder parent ({ feature } as segment) tailSegments kvp =
    if feature == strDefault then
        updateChildrenUnderDefault parent segment tailSegments kvp
    else
        updateChildrenUnderCustom parent segment tailSegments kvp


updateChildrenUnderDefault : Node a -> PathSegment -> List PathSegment -> ( String, String ) -> Node a
updateChildrenUnderDefault ((Node ({ features } as data)) as parent) { index } tailSegments kvp =
    let
        childrenNew =
            getUnderDefault parent
                |> updateChildren index tailSegments kvp

        featuresNew =
            { features | default = childrenNew }
    in
        Node { data | features = featuresNew }


updateChildrenUnderCustom : Node a -> PathSegment -> List PathSegment -> ( String, String ) -> Node a
updateChildrenUnderCustom ((Node ({ features } as data)) as parent) { feature, index } tailSegments kvp =
    let
        childrenNew =
            getUnderCustom feature parent
                |> updateChildren index tailSegments kvp

        featuresNew =
            { features | custom = Dict.insert feature childrenNew features.custom }
    in
        Node { data | features = featuresNew }


updateChildren : Int -> List PathSegment -> ( String, String ) -> List (Node a) -> List (Node a)
updateChildren index tailSegments kvp children =
    let
        updateAt i child =
            if i == index then
                updatePropertyRec child tailSegments kvp
            else
                child
    in
        List.indexedMap updateAt children


updateProperty : Node a -> ( String, String ) -> Node a
updateProperty (Node data) ( key, value ) =
    let
        primitiveOld =
            Dict.get key data.properties
                |> Maybe.withDefault (PString "")

        primitiveNew =
            case primitiveOld of
                PString _ ->
                    PString value

                PInt _ ->
                    String.toInt value
                        |> Maybe.andThen (\i -> Just <| PInt i)
                        |> Maybe.withDefault primitiveOld

                PBool _ ->
                    if String.toLower value == "true" then
                        PBool True
                    else if String.toLower value == "false" then
                        PBool False
                    else
                        primitiveOld
    in
        Node { data | properties = Dict.insert key primitiveNew data.properties }


updatePaths : Node a -> Node a
updatePaths (Node ({ path } as data)) =
    Node { data | features = addFeaturePath path data.features }


addFeaturePath : Path -> Features a -> Features a
addFeaturePath parentPath { default, custom } =
    let
        indexUpdater feature =
            List.indexedMap (addPath parentPath feature)

        defaultNew =
            indexUpdater strDefault default

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


splitLastPathSegment : Path -> ( Maybe PathSegment, Maybe Path )
splitLastPathSegment (Path segments) =
    let
        reversed =
            List.reverse segments
    in
        ( List.head reversed, List.tail reversed |> Maybe.andThen (\t -> Just (Path (List.reverse t))) )



-- TREE NAVIGATION


dropRootSegment : Path -> Path
dropRootSegment ((Path segments) as path) =
    case segments of
        { feature } :: tail ->
            if feature == "root" then
                Path tail
            else
                path

        _ ->
            path


parentOf : Node a -> Path -> Maybe (Node a)
parentOf root path =
    let
        (Path segmentsNoRoot) =
            dropRootSegment path
    in
        List.reverse segmentsNoRoot
            |> List.tail
            |> Maybe.andThen (\t -> Just (List.reverse t))
            |> Maybe.andThen (nodeAt root)


previousSibling : Node a -> Path -> Maybe (Node a)
previousSibling root path =
    sibling root path (-)


nextSibling : Node a -> Path -> Maybe (Node a)
nextSibling root path =
    sibling root path (+)


sibling : Node a -> Path -> (Int -> Int -> Int) -> Maybe (Node a)
sibling root path op =
    let
        (Path segmentsNoRoot) =
            dropRootSegment path

        split =
            splitLastPathSegment (Path segmentsNoRoot)
    in
        case split of
            ( Nothing, _ ) ->
                Nothing

            ( _, Nothing ) ->
                Nothing

            ( Just last, Just (Path parentSegments) ) ->
                let
                    lastNew =
                        { feature = last.feature
                        , index = op last.index 1
                        }
                in
                    nodeAt root (parentSegments ++ [ lastNew ])


nodeAt : Node a -> List PathSegment -> Maybe (Node a)
nodeAt parent segments =
    case segments of
        { feature, index } :: tail ->
            let
                getAt i children =
                    Array.fromList children
                        |> Array.get i

                mbNextChild =
                    getUnder feature parent
                        |> getAt index
            in
                case mbNextChild of
                    Nothing ->
                        Nothing

                    Just child ->
                        nodeAt child tail

        [] ->
            Just parent
