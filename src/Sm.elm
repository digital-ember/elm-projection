module Sm exposing(..)

import Workbench exposing(..)

type alias StateMachine =
  { rootId : Int 
  , events : List Event
  }

type alias Event =
  { name : String }

initialSm =
  { rootId = 0
  , events = [ { name = "" } ]
  }

main =
  tool editor update initialSm  

editor runtime sm =
  constantCell sm "hello projection!"

update runtime sm =
  sm
