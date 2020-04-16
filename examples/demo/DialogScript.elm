module DialogScript exposing (..)

import Color
import Editor as E
import Runtime as R
import Structure as S



-- DOMAIN TYPES


type Domain
    = Script
    | Character
    | Conditional



roleCharacters =
    S.roleFromString "characters"


roleConditionals =
    S.roleFromString "conditionals"


roleShowConditionalsHoriz =
    S.roleFromString "showHoriz"



-- MAIN


main =
    R.projection emptyScript editorRoot


emptyScript =
    S.createRoot Script


editorRoot : S.Node Domain -> S.Node (E.Cell Domain)
editorRoot script =
    E.rootCell
        |> E.with
            (editorScript script)


editorScript script =
    E.vertStackCell
        |> E.with (editorCharacters script)
        |> E.with (editorConditionals script)


editorCharacters script =
    E.horizStackCell
        |> E.with
            (E.constantCell "Characters:"
                |> withKeywordStyle
            )
        |> E.with
            (E.horizStackCellPH "no characters" roleCharacters createCharacter script
                |> E.addSeparator ","
                |> E.addMargin E.Bottom 20
            )


editorConditionals script =
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

        stackCell =
            if isHoriz then
                E.horizStackCell

            else
                E.vertStackCell

        placeholderCell =
            E.placeholderCell "no conditionals"
                |> E.withEffect
                    (E.replacementEffect roleConditionals script createConditional)

        editorInputCell namedNode =
            E.inputCell S.roleName namedNode
                |> E.withEffect (E.insertionEffect namedNode createConditional)
                |> E.withEffect (E.deletionEffect namedNode)

        editorInputList =
            let
                editorInputListI =
                    case S.getUnderCustom roleConditionals script of
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
    E.vertStackCell
        |> E.with
            (E.buttonCell textButton
                |> E.withEffect setPropertyEffect
            )
        |> E.with
            (stackCell
                |> E.with (E.constantCell "Conditionals:" |> withKeywordStyle)
                |> E.with editorInputList
                |> E.addMargin E.Bottom 20
            )


createCharacter =
    S.createNode Character


createConditional =
    S.createNode Conditional


withKeywordStyle =
    E.withStyle (E.styleTextColor Color.darkBlue)
        >> E.withStyle E.styleBold



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
