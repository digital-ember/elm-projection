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


type Node isa
    = Node
        { path : Path
        , isa : isa
        , properties : Dict String Primitive
        , features : Features isa
        }


type Path
    = Path (List PathSegment)


type alias PathSegment =
    { role : Role
    , index : Int
    }


type alias Features isa =
    { default : List (Node isa)
    , custom : Dict String (List (Node isa))
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


isaOf : Node isa -> isa
isaOf (Node { isa }) =
    isa


isasUnderCustom : Role -> Node isa -> List isa
isasUnderCustom role parent =
    let
        children =
            if role == roleEmpty || role == roleDefault then
                getUnderDefault parent

            else
                getUnderCustom role parent
    in
    List.map isaOf children


pathOf : Node isa -> Path
pathOf (Node { path }) =
    path


pathIndiciesAsIdFromNode : Node isa -> Int
pathIndiciesAsIdFromNode (Node { path }) =
    pathIndiciesAsId path


pathIndiciesAsId : Path -> Int
pathIndiciesAsId (Path segments) =
    List.foldl pathIndexAsId "1" segments |> String.toInt |> Maybe.withDefault -1


pathIndexAsId : PathSegment -> String -> String
pathIndexAsId segment idPart =
    idPart
        ++ String.fromInt segment.index


pathAsIdFromNode : Node isa -> String
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
    isaOf l
        == isaOf r
        && pathAsIdFromNode l
        == pathAsIdFromNode r
        && compareProperties mbRoles l r


compareProperties : Maybe (List Role) -> Node isa -> Node isa -> Bool
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


valueOf : Role -> Node isa -> Maybe Primitive
valueOf (Role key) (Node { properties }) =
    Dict.get key properties


tryTextOf : Role -> Node isa -> Maybe String
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


textOf : Role -> Node isa -> String
textOf role node =
    tryTextOf role node
        |> Maybe.withDefault ""


tryIntOf : Role -> Node isa -> Maybe Int
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


intOf : Role -> Node isa -> Int
intOf role node =
    tryIntOf role node
        |> Maybe.withDefault 0


tryFloatOf : Role -> Node isa -> Maybe Float
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


floatOf : Role -> Node isa -> Float
floatOf role node =
    tryFloatOf role node
        |> Maybe.withDefault 0


tryBoolOf : Role -> Node isa -> Maybe Bool
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


boolOf : Role -> Node isa -> Bool
boolOf role node =
    tryBoolOf role node
        |> Maybe.withDefault False



-- NODE CREATION


roleFromString : String -> Role
roleFromString key =
    Role key


emptyFeatures : Features isa
emptyFeatures =
    { default = []
    , custom = Dict.empty
    }


createRoot : isa -> Node isa
createRoot isa =
    createNodeInternal roleRoot isa


createNode : isa -> Node isa
createNode isa =
    createNodeInternal roleEmpty isa


createNodeInternal : Role -> isa -> Node isa
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


addText : Role -> String -> Node isa -> Node isa
addText role text node =
    addProperty ( role, PString text ) node


addInt : Role -> Int -> Node isa -> Node isa
addInt role value node =
    addProperty ( role, PInt value ) node


addFloat : Role -> Float -> Node isa -> Node isa
addFloat role value node =
    addProperty ( role, PFloat value ) node


addBool : Role -> Bool -> Node isa -> Node isa
addBool role value node =
    addProperty ( role, PBool value ) node


addProperty : Property -> Node isa -> Node isa
addProperty ( Role role, value ) (Node data) =
    Node { data | properties = Dict.insert role value data.properties }


addRangeToDefault : List (Node isa) -> Node isa -> Node isa
addRangeToDefault children parent =
    List.foldl addToDefault parent children


addToDefault : Node isa -> Node isa -> Node isa
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


addRangeToCustom : Role -> List (Node isa) -> Node isa -> Node isa
addRangeToCustom role children parent =
    List.foldl (addToCustom role) parent children


addToCustom : Role -> Node isa -> Node isa -> Node isa
addToCustom role child (Node ({ features } as data)) =
    let
        featuresNew =
            { features | custom = updateCustomFeature role child appendTo features.custom }
    in
    Node { data | features = featuresNew }


insertAfterUnderDefault : Node isa -> Path -> Node isa -> Node isa
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


insertAfterUnderCustom : Role -> Node isa -> Path -> Node isa -> Node isa
insertAfterUnderCustom role child pathAfter (Node ({ features } as data)) =
    let
        appender child2 children =
            List.foldl (insertAfter pathAfter child2) [] children

        featuresNew =
            { features | custom = updateCustomFeature role child appender features.custom }
    in
    Node { data | features = featuresNew }


replaceNodeUnderDefault : Node isa -> Path -> Node isa -> Node isa
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


replaceNodeUnderCustom : Role -> Node isa -> Path -> Node isa -> Node isa
replaceNodeUnderCustom role child pathAfter (Node ({ features } as data)) =
    let
        appender child2 children =
            List.foldl (replaceAt pathAfter child2) [] children

        featuresNew =
            { features | custom = updateCustomFeature role child appender features.custom }
    in
    Node { data | features = featuresNew }


insertAfter : Path -> Node isa -> Node isa -> List (Node isa) -> List (Node isa)
insertAfter pathAfter child candidate result =
    if pathAfter == pathOf candidate then
        result ++ [ candidate, child ]

    else
        result ++ [ candidate ]


replaceAt : Path -> Node isa -> Node isa -> List (Node isa) -> List (Node isa)
replaceAt pathAfter child candidate result =
    if pathAfter == pathOf candidate then
        result ++ [ child ]

    else
        result ++ [ candidate ]


getUnder : Role -> Node isa -> List (Node isa)
getUnder role node =
    if role == roleDefault then
        getUnderDefault node

    else
        getUnderCustom role node


getUnderDefault : Node isa -> List (Node isa)
getUnderDefault (Node { features }) =
    features.default


getUnderCustom : Role -> Node isa -> List (Node isa)
getUnderCustom (Role key) (Node { features }) =
    Dict.get key features.custom
        |> Maybe.withDefault []


getAllUnderCustoms : Node isa -> List (Node isa)
getAllUnderCustoms (Node { features }) =
    Dict.values features.custom |> List.concat


addRangeAtPath : Role -> List (Node isa) -> Path -> Node isa -> Node isa
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


addChildAtPath : Role -> Node isa -> Path -> Node isa -> Node isa
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


addChildAtPathRec : Role -> Node isa -> List PathSegment -> Node isa -> Node isa
addChildAtPathRec role nodeNew segments parent =
    case segments of
        [] ->
            if role == roleDefault then
                addToDefault nodeNew parent

            else
                addToCustom role nodeNew parent

        segment :: tail ->
            addChildrenAtPathRec role nodeNew segment tail parent


addChildrenAtPathRec : Role -> Node isa -> PathSegment -> List PathSegment -> Node isa -> Node isa
addChildrenAtPathRec role nodeNew segment tailSegments parent =
    let
        insertAt child =
            let
                (Path pathSegmentsChild) =
                    pathOf child

                mbLastSegment =
                    pathSegmentsChild |> List.reverse |> List.head
            in
            case mbLastSegment of
                Nothing ->
                    child

                Just lastSegment ->
                    if lastSegment.index == segment.index then
                        addChildAtPathRec role nodeNew tailSegments child

                    else
                        child

        childrenNew =
            getUnder segment.role parent
                |> List.map insertAt
    in
    replaceUnderFeature segment.role childrenNew parent


insertChildAfterPath : Node isa -> Path -> Node isa -> Node isa
insertChildAfterPath nodeNew path root =
    let
        (Path segmentsNoRoot) =
            dropRootSegment path
    in
    insertChildAfterPathRec nodeNew path segmentsNoRoot root


insertChildAfterPathRec : Node isa -> Path -> List PathSegment -> Node isa -> Node isa
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


insertChildren : Node isa -> Path -> PathSegment -> List PathSegment -> Node isa -> Node isa
insertChildren nodeNew pathAfter segment tailSegments parent =
    let
        insertAt child =
            let
                (Path pathSegmentsChild) =
                    pathOf child

                mbLastSegment =
                    pathSegmentsChild |> List.reverse |> List.head
            in
            case mbLastSegment of
                Nothing ->
                    child

                Just lastSegment ->
                    if lastSegment.index == segment.index then
                        insertChildAfterPathRec nodeNew pathAfter tailSegments child

                    else
                        child

        childrenNew =
            getUnder segment.role parent
                |> List.map insertAt
    in
    replaceUnderFeature segment.role childrenNew parent


replaceRangeAtPath : Role -> List (Node isa) -> Path -> Node isa -> Node isa
replaceRangeAtPath role rangeNew path root =
    let
        (Path segmentsNoRoot) =
            dropRootSegment path
    in
    replaceRangeAtPathRec role rangeNew path segmentsNoRoot root


replaceRangeAtPathRec : Role -> List (Node isa) -> Path -> List PathSegment -> Node isa -> Node isa
replaceRangeAtPathRec role rangeNew pathAt segments parent =
    case segments of
        [] ->
            replaceUnderFeature role rangeNew parent

        segment :: tail ->
            replaceChildrenForRangeReplace role rangeNew pathAt segment tail parent


replaceChildrenForRangeReplace : Role -> List (Node isa) -> Path -> PathSegment -> List PathSegment -> Node isa -> Node isa
replaceChildrenForRangeReplace role rangeNew pathAt segment tailSegments parent =
    let
        replaceRangeAt child =
            let
                (Path pathSegmentsChild) =
                    pathOf child

                mbLastSegment =
                    pathSegmentsChild |> List.reverse |> List.head
            in
            case mbLastSegment of
                Nothing ->
                    child

                Just lastSegment ->
                    if lastSegment.index == segment.index then
                        replaceRangeAtPathRec role rangeNew pathAt tailSegments child

                    else
                        child

        childrenNew =
            getUnder segment.role parent
                |> List.map replaceRangeAt
    in
    replaceUnderFeature segment.role childrenNew parent


replaceChildAtPath : Node isa -> Path -> Node isa -> Node isa
replaceChildAtPath nodeNew path root =
    let
        (Path segmentsNoRoot) =
            dropRootSegment path
    in
    replaceChildAtPathRec nodeNew path segmentsNoRoot root


replaceChildAtPathRec : Node isa -> Path -> List PathSegment -> Node isa -> Node isa
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


replaceChildrenForChildReplace : Node isa -> Path -> PathSegment -> List PathSegment -> Node isa -> Node isa
replaceChildrenForChildReplace nodeNew pathAt segment tailSegments parent =
    let
        replaceChildAt child =
            let
                (Path pathSegmentsChild) =
                    pathOf child

                mbLastSegment =
                    pathSegmentsChild |> List.reverse |> List.head
            in
            case mbLastSegment of
                Nothing ->
                    child

                Just lastSegment ->
                    if lastSegment.index == segment.index then
                        replaceChildAtPathRec nodeNew pathAt tailSegments child

                    else
                        child

        childrenNew =
            getUnder segment.role parent
                |> List.map replaceChildAt
    in
    replaceUnderFeature segment.role childrenNew parent


deleteNodeUnder : Path -> Node isa -> Node isa
deleteNodeUnder path root =
    let
        (Path segmentsNoRoot) =
            dropRootSegment path
    in
    deleteNodeRec segmentsNoRoot root


deleteNodeRec : List PathSegment -> Node isa -> Node isa
deleteNodeRec segments parent =
    case segments of
        segment :: [] ->
            deleteNodeAt segment parent

        segment :: tail ->
            deleteNodeNested segment tail parent

        [] ->
            parent


deleteNodeNested : PathSegment -> List PathSegment -> Node isa -> Node isa
deleteNodeNested segment tailSegments parent =
    let
        deleteRec child =
            let
                (Path pathSegmentsChild) =
                    pathOf child

                mbLastSegment =
                    pathSegmentsChild |> List.reverse |> List.head
            in
            case mbLastSegment of
                Nothing ->
                    child

                Just lastSegment ->
                    if lastSegment.index == segment.index then
                        deleteNodeRec tailSegments child

                    else
                        child

        childrenNew =
            getUnder segment.role parent
                |> List.map deleteRec
    in
    replaceUnderFeature segment.role childrenNew parent


deleteNodeAt : PathSegment -> Node isa -> Node isa
deleteNodeAt segment parent =
    let
        mbChildAt child =
            let
                (Path pathSegmentsChild) =
                    pathOf child

                mbLastSegment =
                    pathSegmentsChild |> List.reverse |> List.head
            in
            case mbLastSegment of
                Nothing ->
                    Just child

                Just lastSegment ->
                    if lastSegment.index == segment.index then
                        Nothing

                    else
                        Just child

        delete children =
            List.map mbChildAt children
                |> List.filterMap identity

        childrenNew =
            getUnder segment.role parent
                |> delete
    in
    replaceUnderFeature segment.role childrenNew parent


updatePropertyByPath : Node isa -> Path -> ( Role, Primitive ) -> Node isa
updatePropertyByPath root path kvp =
    let
        (Path segmentsNoRoot) =
            dropRootSegment path
    in
    updatePropertyRec segmentsNoRoot kvp root


updatePropertyRec : List PathSegment -> ( Role, Primitive ) -> Node isa -> Node isa
updatePropertyRec segments kvp parent =
    case segments of
        [] ->
            updateProperty kvp parent

        segment :: tail ->
            updateChildrenUnder segment tail kvp parent


updateChildrenUnder : PathSegment -> List PathSegment -> ( Role, Primitive ) -> Node isa -> Node isa
updateChildrenUnder segment tailSegments kvp parent =
    let
        updateAt child =
            let
                (Path pathSegmentsChild) =
                    pathOf child

                mbLastSegment =
                    pathSegmentsChild |> List.reverse |> List.head
            in
            case mbLastSegment of
                Nothing ->
                    child

                Just lastSegment ->
                    if lastSegment.index == segment.index then
                        updatePropertyRec tailSegments kvp child

                    else
                        child

        childrenNew =
            getUnder segment.role parent
                |> List.map updateAt
    in
    replaceUnderFeature segment.role childrenNew parent


updateProperty : ( Role, Primitive ) -> Node isa -> Node isa
updateProperty ( Role key, primitiveNew ) n =
    let
        (Node data) =
            n
    in
    Node { data | properties = Dict.insert key primitiveNew data.properties }


updatePaths : Node isa -> Node isa
updatePaths (Node data) =
    Node { data | features = addFeaturePath data.path data.features }


addFeaturePath : Path -> Features isa -> Features isa
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


ancestorOf : Node isa -> Path -> isa -> Maybe (Node isa)
ancestorOf root path isa =
    case parentOf root path of
        Nothing ->
            Nothing

        Just parent ->
            if isaOf parent == isa then
                Just parent

            else
                ancestorOf root (pathOf parent) isa


parentOf : Node isa -> Path -> Maybe (Node isa)
parentOf root path =
    let
        ( _, mbPathToParent ) =
            dropRootSegment path |> splitLastPathSegment
    in
    mbPathToParent
        |> Maybe.map (\(Path segments) -> segments)
        |> Maybe.andThen (nodeAtI root)


previousSibling : Node isa -> Path -> Maybe (Node isa)
previousSibling root path =
    sibling root path (-)


nextSibling : Node isa -> Path -> Maybe (Node isa)
nextSibling root path =
    sibling root path (+)


sibling : Node isa -> Path -> (Int -> Int -> Int) -> Maybe (Node isa)
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


nodeAt : Node isa -> Path -> Maybe (Node isa)
nodeAt root path =
    let
        (Path segmentsNoRoot) =
            dropRootSegment path
    in
    nodeAtI root segmentsNoRoot


nodeAtI : Node isa -> List PathSegment -> Maybe (Node isa)
nodeAtI parent segments =
    case segments of
        segment :: tail ->
            let
                mbChildAt child =
                    let
                        (Path pathSegmentsChild) =
                            pathOf child

                        mbLastSegment =
                            pathSegmentsChild |> List.reverse |> List.head
                    in
                    case mbLastSegment of
                        Nothing ->
                            Just child

                        Just lastSegment ->
                            if lastSegment.index == segment.index then
                                Just child

                            else
                                Nothing

                getAtIndex children =
                    List.map mbChildAt children
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


nodesOf : isa -> Node isa -> List (Node isa)
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


appendTo : isa -> List isa -> List isa
appendTo child list =
    List.reverse (child :: List.reverse list)


updateCustomFeature : Role -> Node isa -> (Node isa -> List (Node isa) -> List (Node isa)) -> Dict String (List (Node isa)) -> Dict String (List (Node isa))
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


replaceUnderFeature : Role -> List (Node isa) -> Node isa -> Node isa
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
