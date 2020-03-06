module Structure 
  exposing 
    ( Node, Feature, Property 
    , addPaths
    , propertiesOf, featuresOf
    , propertyStringValueOf, propertyIntValueOf, propertyBoolValueOf
    , createRoot, createNode
    , addProperty, addProperties, addText, addInt, addBool
    , addFeature, addFeatures
    , underMain, underText, underCustom, underMainRange
    , getUnderMain, getUnderText, getUnderCustom
    )


import Dict exposing(..)

type Node a =
  Node NodeI


type NodeI
    = NodeI String String (Dict String Property) (List Feature)


type Feature
    = Main (List NodeI)
    | Custom String (List NodeI) 
    | Text (List NodeI)


type Property
    = IntProperty Int
    | StringProperty String
    | BoolProperty Bool



propertyStringValueOf : Node a -> String -> Maybe String
propertyStringValueOf node key =
  let
      props = propertiesOf node
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
      props = propertiesOf node
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
      props = propertiesOf node
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

featuresOf : Node a -> List Feature
featuresOf (Node (NodeI _ _ _ feats)) =
  feats

createRoot : a -> Node a
createRoot _ =
    Node (NodeI "" "root" Dict.empty [])

createNode : a -> Node a
createNode _ =
    Node (NodeI "" "" Dict.empty [])

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
  Node (NodeI name id (Dict.insert key  property properties) features)

addProperties : List Property -> Node a -> Node a
addProperties properties node =
  node --List.foldl addProperty node properties


underMainRange : List (Node a) -> Node b -> Node b
underMainRange children parent =
  List.foldl underMain parent children

underMain : Node a -> Node b -> Node b
underMain child (Node (NodeI name idParent properties features))  =
    Node (NodeI name idParent properties (addToMainFeature child features))

underText : Node a -> Node b -> Node b
underText child  (Node (NodeI name idParent properties features)) =
    Node (NodeI name idParent properties (addToTextFeature child features))

underCustom : String -> Node a -> Node b -> Node b
underCustom customFeatureKey child (Node (NodeI name idParent properties features)) =
    Node (NodeI name idParent properties (addToCustomFeature customFeatureKey child features))


getUnderMain : Node a -> List (Node b)
getUnderMain (Node (NodeI _ _ _ features)) =
    List.foldl 
        (\feature mChildren ->
            case feature of
                Main children ->
                  mChildren ++ (children |> List.map (\ni -> Node ni))
            
                _ ->
                  mChildren
          
        )
        []
        features        
    

getUnderText : Node a -> List (Node b)
getUnderText (Node (NodeI _ _ _ features)) =
    List.foldl 
        (\feature mChildren ->
            case feature of
                Text children ->
                  mChildren ++ (children |> List.map (\ni -> Node ni))
            
                _ ->
                  mChildren
          
        )
        []
        features  

getUnderCustom : String -> Node a -> List (Node b)
getUnderCustom customFeatureKey (Node (NodeI _ _ _ features)) =
    List.foldl 
        (\feature mChildren ->
            case feature of
                Custom key children ->
                    if key == customFeatureKey then
                        mChildren ++ (children |> List.map (\ni -> Node ni))
                    else
                        mChildren
            
                _ ->
                  mChildren
          
        )
        []
        features  
    
addToMainFeature : Node a -> List Feature -> List Feature
addToMainFeature (Node child) features =
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


addToTextFeature : Node a -> List Feature -> List Feature
addToTextFeature (Node child) features =
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

addToCustomFeature : String -> Node a -> List Feature -> List Feature
addToCustomFeature customFeatureKey (Node child) features = 
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
      Custom customFeatureKey textChildren :: otherFeatures  


addFeatures : List Feature -> Node a -> Node a
addFeatures features parent =
  List.foldl addFeature parent features

addFeature : Feature -> Node a -> Node a
addFeature feature parent =
  case feature of
      Main children ->
        List.foldl underMain parent 
          <| List.map (\ni -> Node ni) children

      Text children ->
        List.foldl underText parent 
          <| List.map (\ni -> Node ni) children

      Custom id children ->
        List.foldl (underCustom id) parent 
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