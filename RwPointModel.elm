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
  , animX : Maybe Anim 
  , animY : Maybe Anim 
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
    gen = Random.float 0 360
    (ranDeg, nextSeed) = generate gen seed
    col = hsl (degrees ranDeg) 1 0.5
  in
    (col, nextSeed)


ranColor1 : Seed -> (Color, Seed)
ranColor1 seed = 
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
        , animX = Nothing 
        , animY = Nothing 
        , seed = nextSeed }


    seed = initialSeed (round time)
    seeds = initialSeeds seed 100
    elems = List.map initialElem seeds
  in
    { elems = elems}

updateElem : Inp -> Elem -> Elem
updateElem inp elem = 
  let
    animValue : Anim -> Float
    animValue anim = 
      let
        animEaseValue : Time -> Time -> Float -> Float
        animEaseValue relTime duration to =
          ease easeInExpo Easing.float 0 to duration relTime
    
        relTime = inp.time - anim.startTime
        diff = animEaseValue relTime anim.duration anim.to 
      in 
        anim.startVal + diff
    
        
    updateAnim : Seed -> Float -> Float -> Maybe Anim -> (Maybe Anim, Seed)
    updateAnim seed span value maybeAnim =
      let
        updateJustAnim : Float -> Anim -> (Maybe Anim, Seed)
        updateJustAnim value anim = 
          let
            animReady = (inp.time - anim.startTime) > anim.duration
          in
            if (animReady) then (Nothing, seed)
            else (Just anim, seed)
            

        updateNothingAnim : Float -> Float -> (Maybe Anim, Seed)
        updateNothingAnim span value = 
          let
            maxVal = span / 2 + 150
            minVal = -maxVal
            maxRan = 100
            gen = 
              if (value > maxVal) then Random.float -maxRan 0
              else if (value < minVal) then Random.float 0 maxRan
              else Random.float -maxRan maxRan
            (to, nextSeed) = generate gen seed
            newAnim = 
              { startVal = value
              , startTime = inp.time
              , duration = second * 1
              , to = to}
          in
            (Just newAnim, nextSeed)
        
      in
        case maybeAnim of
          Just anim -> updateJustAnim value anim
          Nothing -> updateNothingAnim span value
          

    updateValue : (Maybe Anim) -> Float -> Float
    updateValue maybeAnim value =
      case maybeAnim of
        Just anim -> animValue anim
        Nothing -> value
    
    nextX = updateValue elem.animX elem.x 
    nextY = updateValue elem.animY elem.y
    (nextAnimX, s1) = updateAnim elem.seed inp.panelSize.w elem.x elem.animX
    (nextAnimY, s2) = updateAnim s1 inp.panelSize.h elem.y elem.animY
  in
    { elem | x = nextX
      , y = nextY
      , animX = nextAnimX
      , animY = nextAnimY
      , seed = s2 }
      

updateModel : Inp -> Maybe Model -> Maybe Model
updateModel inp maybeModel = 
  let
    model = withDefault (initial inp.time) maybeModel
    nextElems = List.map (updateElem inp) model.elems
    nextModel =
      { model | elems = nextElems }
  in 
    Just nextModel