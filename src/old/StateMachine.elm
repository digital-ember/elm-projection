module StateMachine exposing (..)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as JsonD
import List.Extra as ListX
import Task

import Cell exposing(..)
--import Task

main : Program () StateMachine Msg
main =
    Browser.element
        { init = init
        , view = \sm -> view sm
        , update = update
        , subscriptions = \_ -> Sub.none  
        }


-- MODEL
type Ref a =
  Ref { target : a }

type alias Concept a =
  { a | id : String 
  }

type alias Named a =
  Concept { a | name : String }

type alias StateMachine =
  Named 
    { rootId : Int 
    , events : List Event
    , states : List State
    , startStateRef : Ref State
    }

type alias Event =
  Named { status : String }

type alias State =
  Named 
  { transitions : List Transition  
  }

type Transition =
  Transition 
    { event : Ref Event
    , target : Ref State
    }



newStateMachine : StateMachine
newStateMachine = 
  let
    start = startState 0
  in
    { rootId = 0 
    , id = "stateMachine-0"
    , name = "State Machine"
    , events = [newEvent 0]
    , states = [start]
    , startStateRef = Ref { target = start } 
    }


startState : Int -> State 
startState idPart =
  { id = "state-" ++ String.fromInt idPart  
  , name = "Start"
  , transitions = [] 
  }

newEvent : Int -> Event
newEvent idPart =
  { id = "event-" ++ String.fromInt idPart
  , name = "" 
  , status = "default"
  }

init : () -> ( StateMachine, Cmd Msg )
init _ =
  ( newStateMachine, Cmd.none )



-- UPDATE


type Msg 
  = NoOp
  | AddEvent Event
  | UpdateEvent Event String
  | DeleteEvent Event
  | MoveUpEvent Event
  | MoveDownEvent Event


update : Msg -> StateMachine -> ( StateMachine, Cmd Msg )
update msg sm =
  case msg of 
    NoOp ->
      ( sm, Cmd.none )
    
    AddEvent eventSelected ->
      let
        rootIdNew = sm.rootId+1 
        smNew =
          { sm 
          | rootId = rootIdNew
          , events = List.foldl (insertEvent rootIdNew eventSelected) [] sm.events
          } 

        focus = Dom.focus ("event-" ++ String.fromInt rootIdNew)
      in
        ( smNew, Task.attempt (\_ -> NoOp) focus )

    UpdateEvent eventSelected nameNew -> 
      ( { sm | events = List.map (updateEvent eventSelected nameNew) sm.events }, Cmd.none )

    DeleteEvent eventSelected ->
      case sm.events of 
        [] ->
          ( sm, Cmd.none )
        
        head :: tail ->
          case tail of 
            [] -> 
              ( sm, Cmd.none )
            
            _ -> 
              tryDeleteEvent eventSelected sm 

    MoveUpEvent eventSelected -> 
      ( sm, Cmd.none )

    MoveDownEvent eventSelected -> 
      ( sm, Cmd.none )


tryDeleteEvent event sm =
  let
    mbIndex = ListX.elemIndex event sm.events
  in
    case mbIndex of
      Nothing -> 
        ( sm, Cmd.none )
      
      Just index -> 
        let
          eventsNew = List.filter (doNotDelete event) sm.events
          mbEventToFocus = 
            let
              mbe = ListX.getAt index eventsNew 
            in
              case mbe of         
                Just e -> Just e
                
                Nothing -> ListX.last eventsNew 
              
          cmd = 
            case mbEventToFocus of
              Nothing -> Cmd.none
              
              Just eventToFocus ->
                Task.attempt (\_ -> NoOp) (Dom.focus (eventToFocus.id |> Debug.log "ID!") )
        in
          ( { sm | events = eventsNew }, cmd )

doNotDelete : Event -> Event -> Bool
doNotDelete eventSelected event =
  if eventSelected.id == event.id then 
    event.name /= ""  
  else
    True

addEvent : Int -> List Event -> Event -> List Event 
addEvent idPart events eventAfter =
  List.foldl (insertEvent idPart eventAfter) [] events


insertEvent : Int -> Event -> Event -> List Event -> List Event 
insertEvent idPart eventAfter candidate result =
  if candidate.id == eventAfter.id then 
    result ++ [ eventAfter, newEvent idPart ]
  else
    result ++ [ candidate ]

        
updateEvent : Event -> String -> Event -> Event
updateEvent eventToUpdate nameNew eventCandidate =
  if eventToUpdate.id == eventCandidate.id then
    { eventToUpdate | name = nameNew }
  else 
    eventCandidate

splitList : { a | id : String } -> { a | id : String } -> Bool
splitList current candidate =
  current.id == candidate.id


-- VIEW 


view : StateMachine -> Html Msg
view sm =
  div
    [ ]
    [ lazy viewEvents sm.events
    ] 


viewEvents : List Event -> Html Msg
viewEvents events =
  div 
    [ style "font-family" "Consolas" ]
    [ constant 
      [ style "font-weight" "bold" 
      , style "color" "darkblue"
      ] 
      "events"
    , verticalCollection viewEvent events 
    , constant 
      [ style "font-weight" "bold" 
      , style "color" "darkblue"
      ] 
      "end"
    ]



viewEvent : Event -> Html Msg
viewEvent event =
  div 
    [ ]
    [ input 
        [ placeholder "<add event name>"
        , value event.name
        , id event.id
        , onInput (UpdateEvent event)
        , produceKeyboardMsg event
        ]
        []
    ]


produceKeyboardMsg : Event -> Attribute Msg
produceKeyboardMsg event =
  let
    canHandle c =
        case c of 
          13 -> -- ENTER
            JsonD.succeed (AddEvent event)

          8 -> -- BACKSPACE
            JsonD.succeed (DeleteEvent event)  

          38 -> -- UP
            JsonD.succeed (MoveUpEvent event)

          40 -> -- DOWN
            JsonD.succeed (MoveDownEvent event)  
        
          _ ->
            JsonD.fail ("incorrect code: " ++ (String.fromInt c))
  in
      on "keydown" (JsonD.andThen canHandle keyCode)
  


onCode : Int -> Msg -> Attribute Msg
onCode code msg =
    let
        isCode c =
            if (c |> Debug.log "c") == (code |> Debug.log "code") |> Debug.log "equal" then
                JsonD.succeed msg
            else
                JsonD.fail ("not correct code: " ++ (String.fromInt code))
    in
        on "keydown" (JsonD.andThen isCode keyCode)

k_ENTER = 
  13

k_UP = 38

k_DOWN = 40

k_DELETE = 
  8





