module DialogScript2 exposing (..)

import Editor as E
import Runtime as R
import Structure as S


type DialogDomain
    = Script
    | Character
    | Conditional



-- ROLES


roleCharacters =
    S.roleFromString "characters"


roleConditionals =
    S.roleFromString "conditionals"


roleShowConditionalsHoriz =
    S.roleFromString "showHoriz"



-- MAIN


main =
    R.projection testScript editor


emptyScript =
    S.createRoot Script



-- EDITOR


editor script =
    E.rootCell
        |> E.with
            (E.horizStackCell
                |> E.with (editorScript script)
               {- |> E.with
                    (E.vertStackCell
                        |> E.with (E.constantCell "Statistics")
                        |> E.with (E.constantCell "Graphical")
                    )
                    -}
            )


editorScript script =
    let
        isHoriz =
            S.boolOf roleShowConditionalsHoriz script

        textButton =
            if isHoriz then
                "view: 🡇"

            else
                "view: 🡆"

        setPropertyEffect =
            S.asPBool (isHoriz == False)
                |> E.setPropertyEffect script roleShowConditionalsHoriz

        editorCharacters =
            editorLabeledNamedList
                True
                "Characters:"
                "no characters"
                roleCharacters
                createCharacter
                script

        editorConditionals =
            E.vertStackCell
                |> E.with
                    (E.buttonCell textButton
                        |> E.withEffect setPropertyEffect
                    )
                |> E.with
                    (editorLabeledNamedList
                        isHoriz
                        "Conditionals:"
                        "no conditionals"
                        roleConditionals
                        createConditional
                        script
                    )
    in
    E.vertStackCell
        |> E.with editorCharacters
        |> E.with editorConditionals


editorLabeledNamedList isHoriz label placeholderText role creator parent =
    let
        stackCell =
            if isHoriz then
                E.horizStackCell

            else
                E.vertStackCell

        placeholderCell =
            E.placeholderCell placeholderText
                |> E.withEffect
                    (E.replacementEffect role parent creator)

        editorInputCell namedNode =
            E.inputCell S.roleName namedNode
                |> E.withEffect (E.insertionEffect namedNode creator)
                |> E.withEffect (E.deletionEffect namedNode)

        editorInputList =
            let
                editorInputListI =
                    case S.getUnderCustom role parent of
                        [] ->
                            placeholderCell

                        items ->
                            stackCell |> E.withRange (List.map editorInputCell items)
            in
            if isHoriz then
                editorInputListI |> E.addSeparator ","

            else
                editorInputListI |> E.addIndent
    in
    stackCell
        |> E.with (E.constantCell label)
        |> E.with editorInputList
        |> E.addMargin E.Bottom 20


createCharacter =
    S.createNode Character


createConditional =
    S.createNode Conditional



-- DEBUG


testScript =
    emptyScript
        |> S.addRangeToCustom roleCharacters
            [ character "Bill"
            , character "Tom"
            , character "Steve"
            ]
        |> S.addRangeToCustom roleConditionals
            [ conditional "Seen_Carrot"
            , conditional "Dead_Dog"
            , conditional "Talked_Mary_Dog"
            , conditional "Talked_Carrot"
            , conditional "Talked_Dog"
            ]


character name =
    S.createNode Character |> S.addText S.roleName name


conditional name =
    S.createNode Conditional |> S.addText S.roleName name
