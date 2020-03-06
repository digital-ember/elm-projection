module Structure 
  exposing 
    ( Node, Feature, Property 
    , addPaths
    , propertiesOf, featuresOf
    , createRoot, createNode
    , addProperty, addProperties, addText, addInt, addBool
    , addFeature, addFeatures
    , underMain, underText, underCustom
    )


type Node
    = Node String String (List Property) (List Feature)


type Feature
    = Main (List Node)
    | Custom String (List Node) 
    | Text (List Node)


type Property
    = IntProperty String Int
    | StringProperty String String
    | BoolProperty String Bool


propertiesOf : Node -> List Property
propertiesOf (Node _ _ props _) =
  props

featuresOf : Node -> List Feature
featuresOf (Node _ _ _ feats) =
  feats

createRoot : String -> Node
createRoot name =
    Node name "root" [] []

createNode : String -> Node
createNode name =
    Node name "" [] []

addText : String -> String -> Node -> Node
addText id text node =
  addProperty (StringProperty id text) node

addInt : String -> Int -> Node -> Node
addInt id int node =
  addProperty (IntProperty id int) node

addBool : String -> Bool -> Node -> Node
addBool id bool node =
  addProperty (BoolProperty id bool) node

addProperty : Property -> Node -> Node
addProperty property (Node name id properties features) =
  Node name id (property :: properties) features

addProperties : List Property -> Node -> Node
addProperties properties node =
  List.foldl addProperty node properties


underMain : Node -> Node -> Node
underMain child (Node name idParent properties features)  =
    Node name idParent properties (addToMainFeature child features)

underText : Node -> Node -> Node
underText child (Node name idParent properties features) =
    Node name idParent properties (addToTextFeature child features)

underCustom : String -> Node -> Node -> Node
underCustom customFeatureKey child (Node name idParent properties features) =
    Node name idParent properties (addToCustomFeature customFeatureKey child features)

    
addToMainFeature : Node -> List Feature -> List Feature
addToMainFeature child features =
    let
        (mainChildren, otherFeatures) =
            List.foldl 
                  (\feature (mChildren, featuresOther) ->
                      case feature of
                          Main children ->
                            (mChildren ++ children, featuresOther)
                      
                          _ ->
                            (mChildren, feature :: featuresOther)
                            
                    
                  )
                  ([child], [])
                  features
    in
      Main mainChildren :: otherFeatures


addToTextFeature : Node -> List Feature -> List Feature
addToTextFeature child features =
    let
        (textChildren, otherFeatures) =
            List.foldl 
                  (\feature (tChildren, featuresOther) ->
                      case feature of
                          Text children ->
                            (tChildren ++ children, featuresOther)
                      
                          _ ->
                            (tChildren, feature :: featuresOther)
                            
                    
                  )
                  ([child], [])
                  features
    in
      Text textChildren :: otherFeatures  

addToCustomFeature : String -> Node -> List Feature -> List Feature
addToCustomFeature customFeatureKey child features = 
    let
        (textChildren, otherFeatures) =
            List.foldl 
                  (\feature (tChildren, featuresOther) ->
                      case feature of
                          Custom key children ->
                            if key == customFeatureKey then
                              (tChildren ++ children, featuresOther)
                            else
                              (tChildren, feature :: featuresOther)  
                      
                          _ ->
                            (tChildren, feature :: featuresOther)
                            
                    
                  )
                  ([child], [])
                  features
    in
      Text textChildren :: otherFeatures  


addFeatures : List Feature -> Node -> Node
addFeatures features parent =
  List.foldl addFeature parent features

addFeature : Feature -> Node -> Node
addFeature feature parent =
  case feature of
      Main children ->
        List.foldl underMain parent children

      Text children ->
        List.foldl underText parent children

      Custom id children ->
        List.foldl (underCustom id) parent children


addPaths : Node -> Node
addPaths (Node name rootId properties features) =
    let
        rootId2 = if rootId == "" then "root" else rootId
    in
        Node name rootId2 properties (List.map (addFeatureIds rootId2) features)
  

addFeatureIds : String -> Feature -> Feature
addFeatureIds parentId feature =  
    case feature of
        Main children ->
            Main <| List.indexedMap (addIds (parentId ++ ":main")) children

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



addIds : String -> Int -> Node -> Node
addIds path index (Node name _ properties features) =
    let
        idNew =
            path ++ "_" ++ name ++ String.fromInt index
    in
        Node name idNew properties <|
            List.map (addFeatureIds idNew) features