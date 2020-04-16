module Editor.ReflectiveEditor exposing (editorReflection)

import Color
import Editor as E
import Structure as S


editorReflection : ( S.Node a, S.Node (E.Cell a) ) -> S.Node (E.Cell a)
editorReflection ( domainTree, cellTree ) =
    let
        cellTreeContent =
            S.getUnderDefault cellTree |> List.head |> Maybe.withDefault (E.constantCell "nothing to render")
    in
    E.rootCell
        |> E.with
            (E.vertSplitCell
                |> E.with (editorTree domainTree)
                |> E.with (editorTree cellTree)
                |> E.with cellTreeContent
            )


editorTree : S.Node b -> S.Node (E.Cell a)
editorTree tree =
    E.vertStackCell
        |> E.with
            (E.horizStackCell
                |> E.with
                    (E.constantCell "Node"
                        |> withKeywordStyle
                    )
                |> E.with
                    (E.constantCell (Debug.toString <| S.isaOf tree)
                        |> withIsaStyle
                    )
                |> E.addMargin E.Bottom 15
            )
        |> E.with (editorPath tree)
        |> E.with (editorProperties tree)
        |> E.with (editorFeatures tree)
        |> E.setCollapsible


editorPath tree =
    E.horizStackCell
        |> E.with
            (E.constantCell "path:"
                |> withKeywordStyle
                |> E.addMargin E.Bottom 5
            )
        |> E.with (E.constantCell (S.pathAsIdFromNode tree))
        |> E.addIndent
        |> E.addMargin E.Bottom 15


editorProperties tree =
    E.vertStackCell
        |> E.with
            (E.constantCell "properties:"
                |> withKeywordStyle
                |> E.addMargin E.Bottom 5
            )
        |> E.with (editorPropertyList tree)
        |> E.addIndent
        |> E.addMargin E.Bottom 15
        |> E.setCollapsible


editorFeatures tree =
    E.vertStackCell
        |> E.with
            (E.constantCell "features:"
                |> withKeywordStyle
                |> E.addMargin E.Bottom 5
            )
        |> E.with (editorFeature ( "default", S.getUnderDefault tree ))
        |> E.with (editorCustomFeatures tree)
        |> E.addIndent
        |> E.addMargin E.Bottom 15
        |> E.setCollapsible


editorFeature ( feature, children ) =
    let
        editorChildren =
            case children of
                [] ->
                    E.constantCell "<empty feature>"
                        |> withHintStyle
                        |> E.addMargin E.Bottom 15
                        |> E.addIndent

                _ ->
                    E.vertStackCell
                        |> E.withRange (List.map editorTree children)
                        |> E.addMargin E.Bottom 15
                        |> E.addIndent
    in
    E.vertStackCell
        |> E.with
            (E.constantCell (feature ++ ":")
                |> withLabelStyle
                |> E.addMargin E.Bottom 15
            )
        |> E.with editorChildren
        |> E.addIndent
        |> E.setCollapsible


editorCustomFeatures tree =
    let
        editorFeaturesResult =
            case S.customFeatures tree of
                [] ->
                    E.constantCell "<no custom features>"
                        |> withHintStyle

                features ->
                    E.vertStackCell
                        |> E.withRange (List.map editorFeature features)
    in
    E.vertStackCell
        |> E.with editorFeaturesResult


editorPropertyList tree =
    let
        editorPropertiesResult =
            case S.propertiesOf tree of
                [] ->
                    E.constantCell "<no properties>"
                        |> withHintStyle
                        |> E.addMargin E.Bottom 15

                properties ->
                    E.vertGridCell
                        |> E.withRange (List.map editorProperty properties)
    in
    E.vertStackCell
        |> E.with editorPropertiesResult
        |> E.addIndent


editorProperty ( role, primitive ) =
    E.horizStackCell
        |> E.with
            (E.constantCell (role ++ ":") |> withLabelStyle)
        |> E.with
            (E.constantCell <| S.primitiveToString primitive)
        |> E.addMargin E.Bottom 15


withIsaStyle =
    E.withStyle E.styleItalic
        >> E.withStyle (E.styleTextColor Color.darkBrown)

withKeywordStyle =
    E.withStyle E.styleBold
        >> E.withStyle (E.styleTextColor Color.darkBlue)


withHintStyle =
    E.withStyle E.styleItalic
        >> E.withStyle (E.styleTextColor Color.darkGray)


withLabelStyle =
    E.withStyle E.styleBold
            >> E.withStyle (E.styleTextColor Color.darkGreen)
