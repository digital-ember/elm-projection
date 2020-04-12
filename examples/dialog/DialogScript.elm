module DialogScript exposing (..)

import Color
import Editor as E
import Runtime as R
import Structure as S



-- DOMAIN TYPES


type Domain
    = Script
    | Character
    | Condition


roleCharacters =
    S.roleFromString "characters"



-- MAIN


main =
    R.projection emptyScript editorScript


emptyScript =
    S.createRoot Script


editorScript script =
    E.rootCell
        |> E.with
            (E.vertStackCell
                |> E.with (editorCharacters script)
            )


editorCharacters script =
    E.horizStackCell
        |> E.with
            (E.constantCell "Characters:"
                |> withKeywordStyle
            )
        |> E.with (editorCharacterWithPlaceholder script)


editorCharacterWithPlaceholder script =
    let
        createCharacter =
            S.createNode Character

        editorCharacterName character =
            E.inputCell S.roleName character
                |> E.withEffect (E.insertionEffect character createCharacter)
                |> E.withEffect (E.deletionEffect character)
    in
    case S.getUnderCustom roleCharacters script of
        [] ->
            E.placeholderCell "no characters"
                |> E.withEffect (E.replacementEffect roleCharacters script createCharacter)

        charas ->
            E.horizStackCell
                |> E.withRange (List.map editorCharacterName charas)
                |> E.addSeparator ","


withKeywordStyle =
    E.withStyle (E.styleTextColor Color.darkBlue)
        >> E.withStyle E.styleBold
