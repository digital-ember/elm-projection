module Demo exposing(..)

import Pipe as P exposing(..)
import Html.Events exposing(onClick)

type alias Sm =
  { name : String
  , isUpper : Bool
  , events : List Event
  }

type alias Event =
  { name : String 
  }

type Msg 
  = ToggleCase

main =
  P.pipeProgram 
    root
    xform
    appUpdate
  
root = 
  { name = "my statemachine"
  , isUpper = False
  , events = 
    [ { name = "doorClosed" }
    , { name = "panelClosed" }
    ]
  }

xform : Sm -> Cell Msg
xform sm =
  P.vertStackCell []  
    [ P.constantCell [ onClick (P.AppMsg ToggleCase) ] sm.name
    , xformEvents sm.events      
    ]


xformEvents : List Event -> Cell Msg
xformEvents events =
  P.vertStackCell [] 
    (P.constantCell [] "events" :: xformEventNames events ++ [ P.constantCell [] "end" ] )
  

xformEventNames : List Event -> List (Cell Msg)
xformEventNames events =
  List.map (\e -> P.propertyCell [] e.name) events

appUpdate : Msg -> Sm -> Sm
appUpdate msg sm =
  case msg of
    ToggleCase -> 
      if sm.isUpper then 
        { sm | name = String.toLower sm.name, isUpper = False }
      else
        { sm | name = String.toUpper sm.name, isUpper = True }
          
  
