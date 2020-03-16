module Tanks exposing(main)

import Structure exposing(..)
import Editor exposing(..)
import Runtime exposing(..)



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
    createRootCell
        |> with (editorFactionName faction)
        |> with (editorTanks faction)

editorFactionName : Node Domain -> Node (Cell Domain)
editorFactionName faction =
    horizStackCell
        |> with (constantCell "Faction:")
        |> with (inputCell "name" faction)
        |> addMargin Bottom 20


editorTanks : Node Domain -> Node (Cell Domain)
editorTanks faction =
    case  getUnderDefault faction of 
        [] -> 
            editorTanksPlaceholder faction 

        tanks -> 
            List.foldl editorTank vertGridCell tanks

 
editorTanksPlaceholder : Node Domain -> Node (Cell Domain)
editorTanksPlaceholder faction =
    placeholderCell "no tanks"
        |> withEffect (replacementEffect "" faction ctorTank)


editorTank : Node Domain -> Node (Cell Domain) -> Node (Cell Domain)
editorTank tank container =
    container
        |> with (inputCell "name" tank |> withEffect (insertionEffect tank (createNode Tank)))
        |> with (editorTankKind tank)
        |> with (editorTankFireChance tank)
        |> with (editorTankViewRange tank)
        |> with (editorTankSignalRange tank)


editorTankKind : Node Domain -> Node (Cell Domain)
editorTankKind tank =
    horizStackCell
        |> with (constantCell "Kind:")
        |> with (inputCell "kind" tank)

editorTankFireChance : Node Domain -> Node (Cell Domain)
editorTankFireChance tank = 
    horizStackCell
        |> with (constantCell "Chance of fire:")
        |> with (inputCell "fire" tank)
        |> with (constantCell "%")


editorTankViewRange : Node Domain -> Node (Cell Domain)
editorTankViewRange tank = 
    horizStackCell
        |> with (constantCell "View range:")
        |> with (inputCell "vr" tank)
        |> with (constantCell "m")


editorTankSignalRange : Node Domain -> Node (Cell Domain)
editorTankSignalRange tank = 
    horizStackCell
        |> with (constantCell "Signale range:")
        |> with (inputCell "sr" tank)
        |> with (constantCell "m")



-- CTORs

ctorTank : Node Domain 
ctorTank =
    createNode Tank 
    

