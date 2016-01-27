module RwPointModel where

import Time exposing (..)
import Easing exposing (..)
import Signal exposing (..)
import Maybe exposing (..)
import Color exposing (..)
import Random exposing (..)


type alias Elem =
  { x : Float
  , y : Float
  , color : Color
  , anim : Maybe Anim 
  , seed : Seed }
  
  
type alias Model =
  { elems : List Elem }
  
  
type alias Anim =
  { startVal : Float
  , startTime : Time 
  , duration : Time 
  , to : Float }
  
  
type alias PanelSize = 
  { w : Float
  , h : Float }


type alias Inp = 
  { time : Time
  , panelSize : PanelSize }


initialSeeds : Seed -> Int -> List Seed
initialSeeds seed count =
  if count == 0 then []
  else 
    let 
      (i, nextSeed) = generate (Random.int minInt maxInt) seed
      rest = initialSeeds nextSeed (count - 1)
    in
      nextSeed :: rest
      
      
ranColor : Seed -> (Color, Seed)
ranColor seed = 
  let 
    (i, nextSeed) = generate (Random.int 1 3) seed
    color = 
      if (i == 1) then Color.red
      else if (i == 2) then Color.green
      else Color.blue
  in
    (color, nextSeed)

      
initial : Time -> Model
initial time = 
  let
    initialElem : Seed -> Elem
    initialElem seed = 
      let
        (color, nextSeed) = ranColor seed
      in
        { x = 0
        , y = 0
        , color = color
        , anim = Nothing 
        , seed = nextSeed }


    seed = initialSeed (round time)
    seeds = initialSeeds seed 1
    elems = List.map initialElem seeds
  in
    { elems = elems}

updateElem : Inp -> Elem -> Elem
updateElem inp elem = 
  let
    animValue : Time -> Anim -> Float
    animValue time anim= 
      let
        animEaseValue : Time -> Time -> Float -> Float
        animEaseValue relTime duration to =
          let
            from = 0
          in
            ease easeOutCubic Easing.float from to duration relTime
    
    
        relTime = time - anim.startTime
        diff = animEaseValue relTime anim.duration anim.to 
      in 
        anim.startVal + diff
    
        
    updateAnimElem : Time -> Anim -> Elem -> Elem
    updateAnimElem time anim elem = 
      let
        animReady = (time - anim.startTime) > anim.duration
      in
        if (animReady) then
          { elem | x = animValue time anim 
            , anim = Nothing }
        else
          { elem | x = animValue time anim }
      
    
    updateNoAnimElem : Inp -> Elem -> Elem
    updateNoAnimElem inp elem = 
      let 
        maxX = inp.panelSize.w / 2
        minX = -maxX
        gen = 
          if (elem.x > maxX) then Random.float -500 0
          else if (elem.x < minX) then Random.float 0 500
          else Random.float -500 500
        (to, nextSeed) = generate gen elem.seed
        newAnim = 
          { startVal = elem.x 
          , startTime = inp.time
          , duration = second * 5 
          , to = to}
      in
        { elem | seed = nextSeed
          , anim = Just newAnim }
    
  in
    case elem.anim of
      Just anim -> updateAnimElem inp.time anim elem
      Nothing -> updateNoAnimElem inp elem


updateModel : Inp -> Maybe Model -> Maybe Model
updateModel inp maybeModel = 
  let
    model = withDefault (initial inp.time) maybeModel
    nextElems = List.map (updateElem inp) model.elems
    nextModel =
      { model | elems = nextElems }
  in 
    Just nextModel