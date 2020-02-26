module Sm exposing(..)

import Workbench as Wb


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

--editor : Wb.Runtime -> StateMachine -> Wb.Cell
editor runtime sm =
  Wb.makeVertColl 
    [ Wb.constantCell "events" 
    , Wb.vertColl (Wb.propertyCell .name) sm.events 
    , Wb.constantCell "end"
    ]


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