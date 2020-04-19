module Script exposing (..)

import Editor as E
import Runtime as R
import Structure as S



-- DOMAIN TYPE


type Domain
    = Script
    | Character
    | Conditional


main =
    R.projection emptyScript editorRoot


emptyScript =
    S.createRoot Script


editorRoot script =
    E.rootCell
        |> E.with
            (editorScript script)


editorScript script =
    E.constantCell "Hello Script"
