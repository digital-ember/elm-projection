module Tanks exposing (main)

import Editor exposing (..)
import Runtime exposing (..)
import Structure exposing (..)


type Domain
    = Tank
    | Faction


main =
    projection initFaction editorFaction


initFaction : Node Domain
initFaction =
    createRoot Faction


editorFaction : Node Domain -> Node (Cell Domain)
editorFaction faction =
    rootCell
        |> with (editorFactionName faction)
        |> with (editorTanks faction)


editorFactionName : Node Domain -> Node (Cell Domain)
editorFactionName faction =
    horizStackCell
        |> with (constantCell "Faction:")
        |> with (inputCell roleName faction)
        |> addMargin Bottom 20


editorTanks : Node Domain -> Node (Cell Domain)
editorTanks faction =
    case getUnderDefault faction of
        [] ->
            editorTanksPlaceholder faction

        tanks ->
            List.foldl editorTank vertGridCell tanks


editorTanksPlaceholder : Node Domain -> Node (Cell Domain)
editorTanksPlaceholder faction =
    placeholderCell "no tanks"
        |> withEffect (replacementEffect roleDefault faction ctorTank)


editorTank : Node Domain -> Node (Cell Domain) -> Node (Cell Domain)
editorTank tank container =
    container
        |> with (inputCell roleName tank |> withEffect (insertionEffect tank (createNode Tank)))
        |> with (editorTankKind tank)
        |> with (editorTankFireChance tank)
        |> with (editorTankViewRange tank)
        |> with (editorTankSignalRange tank)


editorTankKind : Node Domain -> Node (Cell Domain)
editorTankKind tank =
    horizStackCell
        |> with (constantCell "Kind:")
        |> with (inputCell roleKind tank)


editorTankFireChance : Node Domain -> Node (Cell Domain)
editorTankFireChance tank =
    horizStackCell
        |> with (constantCell "Chance of fire:")
        |> with (inputCell roleFire tank)
        |> with (constantCell "%")


editorTankViewRange : Node Domain -> Node (Cell Domain)
editorTankViewRange tank =
    horizStackCell
        |> with (constantCell "View range:")
        |> with (inputCell roleViewRange tank)
        |> with (constantCell "m")


editorTankSignalRange : Node Domain -> Node (Cell Domain)
editorTankSignalRange tank =
    horizStackCell
        |> with (constantCell "Signale range:")
        |> with (inputCell roleSignalRange tank)
        |> with (constantCell "m")



-- CTORs


ctorTank : Node Domain
ctorTank =
    createNode Tank



-- Roles


roleKind =
    roleFromString "kind"


roleFire =
    roleFromString "fire"


roleViewRange =
    roleFromString "viewRange"


roleSignalRange =
    roleFromString "signalRange"
