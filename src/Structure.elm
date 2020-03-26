module Structure exposing
    ( Node
    , Path(..)
    , Role
    , addBool
    , addChildAtPath
    , addFloat
    , addInt
    , addProperty
    , addRangeAtPath
    , addRangeToCustom
    , addRangeToDefault
    , addText
    , addToCustom
    , addToDefault
    , ancestorOf
    , asPBool
    , asPFloat
    , asPInt
    , asPString
    , boolOf
    , createNode
    , createRoot
    , deleteNodeUnder
    , flatNodeComparer
    , flatNodeListComparer
    , floatOf
    , getUnderCustom
    , getUnderDefault
    , insertChildAfterPath
    , intOf
    , isaOf
    , isasUnderCustom
    , nextSibling
    , nodeAt
    , nodesOf
    , parentOf
    , pathAsId
    , pathAsIdFromNode
    , pathIndiciesAsId
    , pathIndiciesAsIdFromNode
    , pathOf
    , previousSibling
    , replaceChildAtPath
    , replaceRangeAtPath
    , replaceUnderFeature
    , roleDefault
    , roleEmpty
    , roleFromString
    , roleName
    , roleRoot
    , textOf
    , tryBoolOf
    , tryFloatOf
    , tryIntOf
    , tryTextOf
    , updatePaths
    , updatePropertyByPath
    )

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
    { role : Role
    , index : Int
    }


type alias Features a =
    { default : List (Node a)
    , custom : Dict String (List (Node a))
    }


type alias Property =
    ( Role, Primitive )


type Role
    = Role String


type Primitive
    = PFloat Float
    | PInt Int
    | PString String
    | PBool Bool



-- LOOKUPS


roleDefault : Role
roleDefault =
    Role "default"


isaOf : Node a -> a
isaOf (Node { isa }) =
    isa


isasUnderCustom : Role -> Node a -> List a
isasUnderCustom role parent =
    let
        children =
            if role == roleEmpty || role == roleDefault then
                getUnderDefault parent

            else
                getUnderCustom role parent
    in
    List.map isaOf children

pathOf : Node a -> Path
pathOf (Node { path }) =
    path


pathIndiciesAsIdFromNode : Node a -> Int
pathIndiciesAsIdFromNode (Node { path }) =
    pathIndiciesAsId path


pathIndiciesAsId : Path -> Int
pathIndiciesAsId (Path segments) =
    List.foldl pathIndexAsId "1" segments |> String.toInt |> Maybe.withDefault -1


pathIndexAsId : PathSegment -> String -> String
pathIndexAsId segment idPart =
    idPart
        ++ String.fromInt segment.index


pathAsIdFromNode : Node a -> String
pathAsIdFromNode (Node { path }) =
    pathAsId path


pathAsId : Path -> String
pathAsId (Path segments) =
    List.foldl pathSegmentAsId "" segments


pathSegmentAsId : PathSegment -> String -> String
pathSegmentAsId segment idPart =
    let
        idPartWSeparator =
            if idPart == "" then
                ""

            else
                idPart ++ "-"

        (Role feature) =
            segment.role
    in
    idPartWSeparator
        ++ feature
        ++ String.fromInt segment.index


propsOf (Node { properties }) =
    properties


flatNodeListComparer mbRoles lNodes rNodes =
    (List.map2 (flatNodeComparer mbRoles) lNodes rNodes
        |> List.filter (\v -> v == False)
        |> List.length
    )
        == 0


flatNodeComparer mbRoles l r =
    isaOf l -- |> Debug.log "left ")
        == isaOf r --|> Debug.log "right ")
        && pathAsIdFromNode l --|> Debug.log "pathL")
        == pathAsIdFromNode r --|> Debug.log "pathR")
        && compareProperties mbRoles l r


compareProperties : Maybe (List Role) -> Node a -> Node a -> Bool
compareProperties mbRoles l r =
    let
        filterRoles dict =
            case mbRoles of
                Nothing ->
                    dict

                Just roles ->
                    Dict.filter
                        (\k _ ->
                            List.any (\(Role key) -> key == k) roles
                        )
                        dict

        lProps =
            propsOf l |> filterRoles

        rProps =
            propsOf r |> filterRoles
    in
    if (List.length <| Dict.keys lProps) /= (List.length <| Dict.keys rProps) then
        False

    else
        Dict.foldl
            (\k lValue b ->
                if b == False then
                    False

                else
                    let
                        mbRValue =
                            Dict.get k rProps
                    in
                    case mbRValue of
                        Nothing ->
                            False

                        Just rValue ->
                            if rValue /= lValue then
                                False

                            else
                                primitiveToString rValue == primitiveToString lValue
            )
            True
            lProps


primitiveToString p =
    case p of
        PString v ->
            v

        PBool b ->
            if b then
                "True"

            else
                "False"

        PInt i ->
            String.fromInt i

        PFloat f ->
            String.fromFloat f


valueOf : Role -> Node a -> Maybe Primitive
valueOf (Role key) (Node { properties }) =
    Dict.get key properties


tryTextOf : Role -> Node a -> Maybe String
tryTextOf role node =
    valueOf role node
        |> Maybe.andThen
            (\prop ->
                case prop of
                    PString v ->
                        Just v

                    _ ->
                        Nothing
            )


textOf : Role -> Node a -> String
textOf role node =
    tryTextOf role node
        |> Maybe.withDefault ""


tryIntOf : Role -> Node a -> Maybe Int
tryIntOf role node =
    valueOf role node
        |> Maybe.andThen
            (\prop ->
                case prop of
                    PInt v ->
                        Just v

                    _ ->
                        Nothing
            )


intOf : Role -> Node a -> Int
intOf role node =
    tryIntOf role node
        |> Maybe.withDefault 0


tryFloatOf : Role -> Node a -> Maybe Float
tryFloatOf role node =
    valueOf role node
        |> Maybe.andThen
            (\prop ->
                case prop of
                    PFloat v ->
                        Just v

                    _ ->
                        Nothing
            )


floatOf : Role -> Node a -> Float
floatOf role node =
    tryFloatOf role node
        |> Maybe.withDefault 0


tryBoolOf : Role -> Node a -> Maybe Bool
tryBoolOf role node =
    valueOf role node
        |> Maybe.andThen
            (\prop ->
                case prop of
                    PBool v ->
                        Just v

                    _ ->
                        Nothing
            )


boolOf : Role -> Node a -> Bool
boolOf role node =
    tryBoolOf role node
        |> Maybe.withDefault False



-- NODE CREATION


roleFromString : String -> Role
roleFromString key =
    Role key


emptyFeatures : Features a
emptyFeatures =
    { default = []
    , custom = Dict.empty
    }


createRoot : a -> Node a
createRoot isa =
    createNodeInternal roleRoot isa


createNode : a -> Node a
createNode isa =
    createNodeInternal roleEmpty isa


createNodeInternal : Role -> a -> Node a
createNodeInternal role isa =
    Node
        { isa = isa
        , path = Path [ { role = role, index = 0 } ]
        , properties = Dict.empty
        , features = emptyFeatures
        }


asPString : String -> Primitive
asPString s =
    PString s


asPInt : Int -> Primitive
asPInt i =
    PInt i


asPBool : Bool -> Primitive
asPBool b =
    PBool b


asPFloat : Float -> Primitive
asPFloat f =
    PFloat f


addText : Role -> String -> Node a -> Node a
addText role text node =
    addProperty ( role, PString text ) node


addInt : Role -> Int -> Node a -> Node a
addInt role value node =
    addProperty ( role, PInt value ) node


addFloat : Role -> Float -> Node a -> Node a
addFloat role value node =
    addProperty ( role, PFloat value ) node


addBool : Role -> Bool -> Node a -> Node a
addBool role value node =
    addProperty ( role, PBool value ) node


addProperty : Property -> Node a -> Node a
addProperty ( Role role, value ) (Node data) =
    Node { data | properties = Dict.insert role value data.properties }


addRangeToDefault : List (Node a) -> Node a -> Node a
addRangeToDefault children parent =
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


addRangeToCustom : Role -> List (Node a) -> Node a -> Node a
addRangeToCustom role children parent =
    List.foldl (addToCustom role) parent children


addToCustom : Role -> Node a -> Node a -> Node a
addToCustom role child (Node ({ features } as data)) =
    let
        featuresNew =
            { features | custom = updateCustomFeature role child appendTo features.custom }
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


insertAfterUnderCustom : Role -> Node a -> Path -> Node a -> Node a
insertAfterUnderCustom role child pathAfter (Node ({ features } as data)) =
    let
        appender child2 children =
            List.foldl (insertAfter pathAfter child2) [] children

        featuresNew =
            { features | custom = updateCustomFeature role child appender features.custom }
    in
    Node { data | features = featuresNew }


replaceNodeUnderDefault : Node a -> Path -> Node a -> Node a
replaceNodeUnderDefault child pathAfter (Node ({ features } as data)) =
    let
        featuresNew =
            case features.default of
                [] ->
                    { features | default = [ child ] }

                children ->
                    { features | default = List.foldl (replaceAt pathAfter child) [] children }
    in
    Node { data | features = featuresNew }


replaceNodeUnderCustom : Role -> Node a -> Path -> Node a -> Node a
replaceNodeUnderCustom role child pathAfter (Node ({ features } as data)) =
    let
        appender child2 children =
            List.foldl (replaceAt pathAfter child2) [] children

        featuresNew =
            { features | custom = updateCustomFeature role child appender features.custom }
    in
    Node { data | features = featuresNew }


insertAfter : Path -> Node a -> Node a -> List (Node a) -> List (Node a)
insertAfter pathAfter child candidate result =
    if pathAfter == pathOf candidate then
        result ++ [ candidate, child ]

    else
        result ++ [ candidate ]


replaceAt : Path -> Node a -> Node a -> List (Node a) -> List (Node a)
replaceAt pathAfter child candidate result =
    if pathAfter == pathOf candidate then
        result ++ [ child ]

    else
        result ++ [ candidate ]


getUnder : Role -> Node a -> List (Node a)
getUnder role node =
    if role == roleDefault then
        getUnderDefault node

    else
        getUnderCustom role node


getUnderDefault : Node a -> List (Node a)
getUnderDefault (Node { features }) =
    features.default


getUnderCustom : Role -> Node a -> List (Node a)
getUnderCustom (Role key) (Node { features }) =
    Dict.get key features.custom
        |> Maybe.withDefault []


getAllUnderCustoms : Node a -> List (Node a)
getAllUnderCustoms (Node { features }) =
    Dict.values features.custom |> List.concat


addRangeAtPath : Role -> List (Node a) -> Path -> Node a -> Node a
addRangeAtPath role range ((Path segments) as path) root =
    let
        (Role feature) =
            role

        pathWithOneChild =
            appendToPath ( feature, 0 ) path

        add c r =
            case getUnder role r of
                [] ->
                    addChildAtPath role c path r

                _ ->
                    insertChildAfterPath c pathWithOneChild r
    in
    List.foldl add root range


addChildAtPath : Role -> Node a -> Path -> Node a -> Node a
addChildAtPath role nodeNew path root =
    let
        (Path segmentsNoRoot) =
            dropRootSegment path

        roleToAdd =
            if role == roleEmpty then
                roleDefault

            else
                role
    in
    addChildAtPathRec roleToAdd nodeNew segmentsNoRoot root


addChildAtPathRec : Role -> Node a -> List PathSegment -> Node a -> Node a
addChildAtPathRec role nodeNew segments parent =
    case segments of
        [] ->
            if role == roleDefault then
                addToDefault nodeNew parent

            else
                addToCustom role nodeNew parent

        segment :: tail ->
            addChildrenAtPathRec role nodeNew segment tail parent


addChildrenAtPathRec : Role -> Node a -> PathSegment -> List PathSegment -> Node a -> Node a
addChildrenAtPathRec role nodeNew segment tailSegments parent =
    let
        insertAt i child =
            if i == segment.index then
                addChildAtPathRec role nodeNew tailSegments child

            else
                child

        childrenNew =
            getUnder segment.role parent
                |> List.indexedMap insertAt
    in
    replaceUnderFeature segment.role childrenNew parent


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
        { role } :: [] ->
            if role == roleDefault then
                insertAfterUnderDefault nodeNew pathAfter parent

            else
                insertAfterUnderCustom role nodeNew pathAfter parent

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
            getUnder segment.role parent
                |> List.indexedMap insertAt
    in
    replaceUnderFeature segment.role childrenNew parent


replaceRangeAtPath : Role -> List (Node a) -> Path -> Node a -> Node a
replaceRangeAtPath role rangeNew path root =
    let
        (Path segmentsNoRoot) =
            dropRootSegment path
    in
    replaceRangeAtPathRec role rangeNew path segmentsNoRoot root


replaceRangeAtPathRec : Role -> List (Node a) -> Path -> List PathSegment -> Node a -> Node a
replaceRangeAtPathRec role rangeNew pathAt segments parent =
    case segments of
        [] ->
            replaceUnderFeature role rangeNew parent

        segment :: tail ->
            replaceChildrenForRangeReplace role rangeNew pathAt segment tail parent


replaceChildrenForRangeReplace : Role -> List (Node a) -> Path -> PathSegment -> List PathSegment -> Node a -> Node a
replaceChildrenForRangeReplace role rangeNew pathAt segment tailSegments parent =
    let
        replaceRangeAt i child =
            if i == segment.index then
                replaceRangeAtPathRec role rangeNew pathAt tailSegments child

            else
                child

        childrenNew =
            getUnder segment.role parent
                |> List.indexedMap replaceRangeAt
    in
    replaceUnderFeature segment.role childrenNew parent


replaceChildAtPath : Node a -> Path -> Node a -> Node a
replaceChildAtPath nodeNew path root =
    let
        (Path segmentsNoRoot) =
            dropRootSegment path
    in
    replaceChildAtPathRec nodeNew path segmentsNoRoot root


replaceChildAtPathRec : Node a -> Path -> List PathSegment -> Node a -> Node a
replaceChildAtPathRec nodeNew pathAt segments parent =
    case segments of
        { role } :: [] ->
            if role == roleDefault then
                replaceNodeUnderDefault nodeNew pathAt parent

            else
                replaceNodeUnderCustom role nodeNew pathAt parent

        segment :: tail ->
            replaceChildrenForChildReplace nodeNew pathAt segment tail parent

        _ ->
            parent


replaceChildrenForChildReplace : Node a -> Path -> PathSegment -> List PathSegment -> Node a -> Node a
replaceChildrenForChildReplace nodeNew pathAt segment tailSegments parent =
    let
        replaceChildAt i child =
            if i == segment.index then
                replaceChildAtPathRec nodeNew pathAt tailSegments child

            else
                child

        childrenNew =
            getUnder segment.role parent
                |> List.indexedMap replaceChildAt
    in
    replaceUnderFeature segment.role childrenNew parent


deleteNodeUnder : Path -> Node a -> Node a
deleteNodeUnder path root =
    let
        (Path segmentsNoRoot) =
            dropRootSegment path
    in
    deleteNodeRec segmentsNoRoot root


deleteNodeRec : List PathSegment -> Node a -> Node a
deleteNodeRec segments parent =
    case segments of
        segment :: [] ->
            deleteNodeAt segment parent

        segment :: tail ->
            deleteNodeNested segment tail parent

        [] ->
            parent


deleteNodeNested : PathSegment -> List PathSegment -> Node a -> Node a
deleteNodeNested segment tailSegments parent =
    let
        deleteRec i child =
            if i == segment.index then
                deleteNodeRec tailSegments child

            else
                child

        childrenNew =
            getUnder segment.role parent
                |> List.indexedMap deleteRec
    in
    replaceUnderFeature segment.role childrenNew parent


deleteNodeAt : PathSegment -> Node a -> Node a
deleteNodeAt segment parent =
    let
        mbChildAt i c =
            if i == segment.index then
                Nothing

            else
                Just c

        delete children =
            List.indexedMap mbChildAt children
                |> List.filterMap identity

        childrenNew =
            getUnder segment.role parent
                |> delete
    in
    replaceUnderFeature segment.role childrenNew parent


updatePropertyByPath : Node a -> Path -> ( Role, Primitive ) -> Node a
updatePropertyByPath root path kvp =
    let
        (Path segmentsNoRoot) =
            dropRootSegment path
    in
    updatePropertyRec segmentsNoRoot kvp root


updatePropertyRec : List PathSegment -> ( Role, Primitive ) -> Node a -> Node a
updatePropertyRec segments kvp parent =
    case segments of
        [] ->
            updateProperty kvp parent

        segment :: tail ->
            updateChildrenUnder segment tail kvp parent


updateChildrenUnder : PathSegment -> List PathSegment -> ( Role, Primitive ) -> Node a -> Node a
updateChildrenUnder segment tailSegments kvp parent =
    let
        updateAt i child =
            if i == segment.index then
                updatePropertyRec tailSegments kvp child

            else
                child

        childrenNew =
            getUnder segment.role parent
                |> List.indexedMap updateAt
    in
    replaceUnderFeature segment.role childrenNew parent


updateProperty : ( Role, Primitive ) -> Node a -> Node a
updateProperty ( Role key, primitiveNew ) (Node data) =
    Node { data | properties = Dict.insert key primitiveNew data.properties }


updatePaths : Node a -> Node a
updatePaths (Node data) =
    Node { data | features = addFeaturePath data.path data.features }


addFeaturePath : Path -> Features a -> Features a
addFeaturePath parentPath { default, custom } =
    let
        addPath (Path parentSegments) feature index (Node data) =
            let
                segmentNew =
                    { role = Role feature, index = index }

                pathNew =
                    Path <|
                        appendTo segmentNew parentSegments
            in
            Node { data | path = pathNew, features = addFeaturePath pathNew data.features }

        indexUpdater feature =
            List.indexedMap (addPath parentPath feature)

        (Role strDefault) =
            roleDefault

        defaultNew =
            indexUpdater strDefault default

        customNew =
            Dict.map indexUpdater custom
    in
    { default = defaultNew
    , custom = customNew
    }


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


appendToPath : ( String, Int ) -> Path -> Path
appendToPath ( feature, index ) (Path segments) =
    Path <|
        appendTo (PathSegment (Role feature) index) segments



-- TREE NAVIGATION


ancestorOf : Node a -> Path -> a -> Maybe (Node a)
ancestorOf root path isa =
    let
        mbParent =
            parentOf root path
    in
    case mbParent of
        Nothing ->
            Nothing

        Just parent ->
            if isaOf parent == isa then
                Just parent

            else
                ancestorOf root (pathOf parent) isa


parentOf : Node a -> Path -> Maybe (Node a)
parentOf root path =
    let
        ( _, mbPathToParent ) =
            dropRootSegment path |> splitLastPathSegment
    in
    mbPathToParent
        |> Maybe.map (\(Path segments) -> segments)
        |> Maybe.andThen (nodeAtI root)


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
                    { role = last.role
                    , index = op last.index 1
                    }
            in
            nodeAtI root (parentSegments ++ [ lastNew ])


nodeAt : Node a -> Path -> Maybe (Node a)
nodeAt parent path =
    let
        (Path segmentsNoRoot) =
            dropRootSegment path
    in
    nodeAtI parent segmentsNoRoot


nodeAtI : Node a -> List PathSegment -> Maybe (Node a)
nodeAtI parent segments =
    case segments of
        segment :: tail ->
            let
                mbChildAt i c =
                    if i == segment.index then
                        Just c

                    else
                        Nothing

                getAtIndex children =
                    List.indexedMap mbChildAt children
                        |> List.filterMap identity

                nextChild =
                    getUnder segment.role parent
                        |> getAtIndex
            in
            case nextChild of
                child :: [] ->
                    nodeAtI child tail

                _ ->
                    Nothing

        [] ->
            Just parent


nodesOf : a -> Node a -> List (Node a)
nodesOf isa root =
    nodesOfRec isa root []


nodesOfRec isa node result =
    case isaOf node == isa of
        True ->
            node :: result

        False ->
            let
                allChildren =
                    getUnderDefault node
                        |> List.append (getAllUnderCustoms node)
            in
            List.foldl (nodesOfRec isa) result allChildren



-- UTILITIES


appendTo : a -> List a -> List a
appendTo child list =
    List.reverse (child :: List.reverse list)


updateCustomFeature : Role -> Node a -> (Node a -> List (Node a) -> List (Node a)) -> Dict String (List (Node a)) -> Dict String (List (Node a))
updateCustomFeature (Role key) child appender custom =
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


replaceUnderFeature : Role -> List (Node a) -> Node a -> Node a
replaceUnderFeature role childrenNew (Node ({ features } as data)) =
    let
        (Role key) =
            role

        featuresNew =
            if role == roleDefault then
                { features | default = childrenNew }

            else
                { features | custom = Dict.insert key childrenNew features.custom }
    in
    Node { data | features = featuresNew }


dropRootSegment : Path -> Path
dropRootSegment ((Path segments) as path) =
    case segments of
        { role } :: tail ->
            if role == roleRoot then
                Path tail

            else
                path

        _ ->
            path


roleName =
    roleFromString "name"


roleRoot =
    roleFromString "root"


roleEmpty =
    roleFromString ""
