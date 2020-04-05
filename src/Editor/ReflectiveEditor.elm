module Editor.ReflectiveEditor exposing (editorReflection)

import Editor as E
import Structure as S


editorReflection : S.Node a -> S.Node (E.Cell a)
editorReflection tree =
    E.rootCell
        |> E.with (editorTree tree)


editorTree : S.Node a -> S.Node (E.Cell a)
editorTree tree =
    E.vertStackCell
        |> E.with
            (E.constantCell ("Node " ++ (Debug.toString <| S.isaOf tree))
                |> E.addMargin E.Bottom 15
            )
        |> E.with (editorProperties tree)
        |> E.with (editorFeatures tree)


editorProperties tree =
    E.vertStackCell
        |> E.with (E.constantCell "properties:" |> E.addMargin E.Bottom 5)
        |> E.with (editorPropertyList tree)
        |> E.addIndent
        |> E.addMargin E.Bottom 15


editorFeatures tree =
    E.vertStackCell
        |> E.with (E.constantCell "features:" |> E.addMargin E.Bottom 5)
        |> E.with (editorFeature ( "default", S.getUnderDefault tree ))
        |> E.with (editorCustomFeatures tree)
        |> E.addIndent
        |> E.addMargin E.Bottom 15


editorFeature ( feature, children ) =
    let
        editorChildren =
            case children of
                [] ->
                    E.constantCell "<empty feature>"
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
            (E.constantCell (feature ++ ":") |> E.addMargin E.Bottom 15)
        |> E.with editorChildren
        |> E.addIndent


editorCustomFeatures tree =
    let
        editorFeaturesResult =
            case S.customFeatures tree of
                [] ->
                    E.constantCell "<no custom features>"

                features ->
                    E.vertStackCell
                        |> E.withRange (List.map editorFeature features)
    in
    E.vertStackCell
        |> E.with editorFeaturesResult
        |> E.addIndent


editorPropertyList tree =
    let
        editorPropertiesResult =
            case S.propertiesOf tree of
                [] ->
                    E.constantCell "<no properties>" |> E.addMargin E.Bottom 15

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
            (E.constantCell (role ++ ":"))
        |> E.with
            (E.constantCell <| S.primitiveToString primitive)
        |> E.addMargin E.Bottom 15
