module Editor exposing(..)

type Cell =
  Cell
    { id : String
    , mbParent : Maybe Cell 
    }
    KCell

type KCell
  = ConstantCell String
  | PropertyCell String
  | StackCell StackOrientation (List Cell)

type StackOrientation 
  = Horiz
  | Vert


constCell : String -> Maybe Cell -> Cell
constCell txt mbParent =
  Cell { id = "", mbParent = mbParent } (ConstantCell txt)

propertyCell : String -> Maybe Cell -> Cell
propertyCell txt mbParent =
  Cell { id = "", mbParent = mbParent } (PropertyCell txt)

verticalStack : Maybe Cell -> (a -> Cell) -> List a -> Cell
verticalStack mbParent toCell things =
  let
    cells = List.map toCell things
  in
    Cell { id = "", mbParent = mbParent } (StackCell Vert cells)
