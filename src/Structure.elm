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
    , replaceUnderFeature
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
insertChildren nodeNew pathAfter segment tailSegments parent =
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
        segment :: [] ->
            deleteNodeUnder segment parent

        segment :: tail ->
            deleteNodeNested segment tail parent

        [] ->
            parent


deleteNodeNested : PathSegment -> List PathSegment -> Node a -> Node a
deleteNodeNested { feature, index } tailSegments parent =
    let
        deleteRec i child =
            if i == index then
                deleteNodeRec tailSegments child

            else
                child

        childrenNew =
            getUnder feature parent
                |> List.indexedMap deleteRec
    in
    replaceUnderFeature feature childrenNew parent


deleteNodeUnder : PathSegment -> Node a -> Node a
deleteNodeUnder { feature, index } parent =
    let
        mbChildAt i c =
            if i == index then
                Nothing

            else
                Just c

        delete children =
            List.indexedMap mbChildAt children
                |> List.filterMap identity

        childrenNew =
            getUnder feature parent
                |> delete
    in
    replaceUnderFeature feature childrenNew parent


updatePropertyByPath : Node a -> Path -> ( String, String ) -> Node a
updatePropertyByPath root path kvp =
    let
        (Path segmentsNoRoot) =
            dropRootSegment path
    in
    updatePropertyRec segmentsNoRoot kvp root


updatePropertyRec : List PathSegment -> ( String, String ) -> Node a -> Node a
updatePropertyRec segments kvp parent =
    case segments of
        [] ->
            updateProperty kvp parent

        segment :: tail ->
            updateChildrenUnder segment tail kvp parent


updateChildrenUnder : PathSegment -> List PathSegment -> ( String, String ) -> Node a -> Node a
updateChildrenUnder { feature, index } tailSegments kvp parent =
    let
        updateAt i child =
            if i == index then
                updatePropertyRec tailSegments kvp child

            else
                child

        childrenNew =
            getUnder feature parent
                |> List.indexedMap updateAt
    in
    replaceUnderFeature feature childrenNew parent


updateProperty : ( String, String ) -> Node a -> Node a
updateProperty ( key, value ) (Node data) =
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
updatePaths (Node data) =
    Node { data | features = addFeaturePath data.path data.features }


addFeaturePath : Path -> Features a -> Features a
addFeaturePath parentPath { default, custom } =
    let
        indexUpdater feature =
            List.indexedMap (addPath parentPath feature)

        defaultNew =
            indexUpdater strDefault default

        customNew =
            Dict.map indexUpdater custom
    in
    { default = defaultNew
    , custom = customNew
    }


addPath : Path -> String -> Int -> Node a -> Node a
addPath (Path parentSegments) feature index (Node data) =
    let
        segmentNew =
            { feature = feature, index = index }

        pathNew =
            Path <|
                appendTo segmentNew parentSegments
    in
    Node { data | path = pathNew, features = addFeaturePath pathNew data.features }


splitLastPathSegment : Path -> ( Maybe PathSegment, Maybe Path )
splitLastPathSegment (Path segments) =
    let
        reversed =
            List.reverse segments

        tailReversed t =
            Path (List.reverse t)
    in
    case reversed of
        [] ->
            ( Nothing, Nothing )

        head :: [] ->
            ( Just head, Nothing )

        head :: tail ->
            ( Just head, Just <| tailReversed tail )



-- TREE NAVIGATION


parentOf : Node a -> Path -> Maybe (Node a)
parentOf root path =
    let
        ( _, mbPathToParent ) =
            dropRootSegment path |> splitLastPathSegment
    in
    mbPathToParent
        |> Maybe.map (\(Path segments) -> segments)
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
                mbChildAt i c =
                    if i == index then
                        Just c

                    else
                        Nothing

                getAtIndex children =
                    List.indexedMap mbChildAt children
                        |> List.filterMap identity

                nextChild =
                    getUnder feature parent
                        |> getAtIndex
            in
            case nextChild of
                child :: [] ->
                    nodeAt child tail

                _ ->
                    Nothing

        [] ->
            Just parent



-- UTILITIES


appendTo : a -> List a -> List a
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
replaceUnderFeature feature childrenNew (Node ({ features } as data)) =
    let
        featuresNew =
            if feature == strDefault then
                { features | default = childrenNew }

            else
                { features | custom = Dict.insert feature childrenNew features.custom }
    in
    Node { data | features = featuresNew }


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
