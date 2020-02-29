module Sm2 exposing(..)

import Workbench2 as Wb


-- MODEL (just a partial state machine atm)

type alias StateMachine =
  { events : List Event
  }

type alias Event =
  { name : String 
  }


-- MAIN

main =
  Wb.tool editor update init  


init : StateMachine
init =
  { events = dummyEvents
  }


-- EDITOR

editor : Wb.Runtime -> StateMachine -> Wb.Cell
editor runtime sm =
  let
    constCellEvents = constCell "events" Nothing
    stackCellEvents = verticalStack Nothing 
    constCellEnd = constCell "end" Nothing
  in


eventEditor : Event -> Wb.Cell
eventEditor event =
  propertyCell event.name Nothing

-- UPDATE
  
update runtime sm =
  sm


-- HELPERS

dummyEvents : List Event
dummyEvents =
  [ event "doorClosed", event "panelClosed" ]

event : String -> Event
event name =
  { name = name }