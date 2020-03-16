module Structure exposing
    ( Node
    , Path(..)
    , addBool
    , addChildAtPath
    , addInt
    , addProperty
    , addText
    , addToCustom
    , addToDefault
    , addToDefaultRange
    , boolOf
    , createNode
    , createRoot
    , deleteNode
    , getUnderCustom
    , getUnderDefault
    , insertAfterUnderCustom
    , insertAfterUnderDefault
    , insertChildAfterPath
    , intOf
    , isaOf
    , nextSibling
    , parentOf
    , pathAsIdFromNode
    , pathOf
    , previousSibling
    , replaceUnderCustom
    , replaceUnderDefault
    , textOf
    , updatePaths
    , updatePropertyByPath
    )

import Array as Array
import Dict as Dict exposing (Dict)
import Maybe as Maybe


type Node a
    = Node
        { path : Path
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



-- LOOKUPS


strDefault : String
strDefault =
    "default"


isaOf : Node a -> a
isaOf (Node { isa }) =
    isa


pathOf : Node a -> Path
pathOf (Node { path }) =
    path


pathAsIdFromNode : Node a -> String
pathAsIdFromNode (Node { path }) =
    pathAsId path


pathAsId : Path -> String
pathAsId (Path segments) =
    List.foldl pathSegmentAsId "" segments


pathSegmentAsId : PathSegment -> String -> String
pathSegmentAsId { feature, index } idPart =
    let
        idPartWSeparator =
            if idPart == "" then
                ""

            else
                idPart ++ "-"
    in
    idPartWSeparator
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



-- NODE CREATION


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
createNodeInternal feature isa =
    Node
        { isa = isa
        , path = Path [ { feature = feature, index = 0 } ]
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
                    { features | default = appendTo child children }
    in
    Node { data | features = featuresNew }


addToCustom : String -> Node a -> Node a -> Node a
addToCustom key child (Node ({ features } as data)) =
    let
        featuresNew =
            { features | custom = updateCustomFeature key child appendTo features.custom }
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
        appender child2 children =
            List.foldl (insertAfter pathAfter child2) [] children

        featuresNew =
            { features | custom = updateCustomFeature key child appender features.custom }
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


addChildAtPath : String -> Node a -> Path -> Node a -> Node a
addChildAtPath key nodeNew path root =
    let
        (Path segmentsNoRoot) =
            dropRootSegment path

        feature =
            if key == "" then
                strDefault

            else
                key
    in
    addChildAtPathRec feature nodeNew segmentsNoRoot root


addChildAtPathRec : String -> Node a -> List PathSegment -> Node a -> Node a
addChildAtPathRec key nodeNew segments parent =
    case segments of
        [] ->
            if key == strDefault then
                addToDefault nodeNew parent

            else
                addToCustom key nodeNew parent

        segment :: tail ->
            addChildrenAtPathRec key nodeNew segment tail parent


addChildrenAtPathRec : String -> Node a -> PathSegment -> List PathSegment -> Node a -> Node a
addChildrenAtPathRec key nodeNew { feature, index } tailSegments parent =
    let
        insertAt i child =
            if i == index then
                addChildAtPathRec key nodeNew tailSegments child

            else
                child

        childrenNew =
            getUnder feature parent
                |> List.indexedMap insertAt
    in
    replaceUnderFeature feature childrenNew parent


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
        { feature } :: [] ->
            if feature == strDefault then
                insertAfterUnderDefault nodeNew pathAfter parent

            else
                insertAfterUnderCustom feature nodeNew pathAfter parent

        segment :: tail ->
            insertChildren nodeNew pathAfter segment tail parent

        _ ->
            parent


insertChildren : Node a -> Path -> PathSegment -> List PathSegment -> Node a -> Node a
insertChildren nodeNew pathAfter segment tailSegments ((Node ({ features } as data)) as parent) =
    let
        insertAt i child =
            if i == segment.index then
                insertChildAfterPathRec nodeNew pathAfter tailSegments child

            else
                child

        childrenNew =
            getUnder segment.feature parent
                |> List.indexedMap insertAt
    in
    replaceUnderFeature segment.feature childrenNew parent


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
        ({ feature, index } as segment) :: [] ->
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
            List.indexedMap
                (\i c ->
                    if i == index then
                        Nothing

                    else
                        Just c
                )
                children
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
            List.indexedMap
                (\i c ->
                    if i == index then
                        Nothing

                    else
                        Just c
                )
                children
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



-- UTILITIES


appendTo : Node a -> List (Node a) -> List (Node a)
appendTo child list =
    List.reverse (child :: List.reverse list)


updateCustomFeature : String -> Node a -> (Node a -> List (Node a) -> List (Node a)) -> Dict String (List (Node a)) -> Dict String (List (Node a))
updateCustomFeature key child appender custom =
    let
        updater mbChildren =
            Just <|
                case mbChildren of
                    Nothing ->
                        [ child ]

                    Just children ->
                        appender child children
    in
    Dict.update key updater custom


replaceUnderFeature : String -> List (Node a) -> Node a -> Node a
replaceUnderFeature feature defaultNew (Node ({ features } as data)) =
    let
        featuresNew =
            if feature == strDefault then
                { features | default = defaultNew }

            else
                { features | custom = Dict.insert feature defaultNew features.custom }
    in
    Node { data | features = featuresNew }


replaceUnderDefault : List (Node a) -> Node a -> Node a
replaceUnderDefault children node =
    replaceUnderFeature strDefault children node


replaceUnderCustom : String -> List (Node a) -> Node a -> Node a
replaceUnderCustom key children node =
    replaceUnderFeature key children node
