module RwEasing1Model where

import Time exposing (..)
import Easing exposing (..)
import Signal exposing (..)
import Maybe exposing (..)
import Color exposing (..)
import Random exposing (..)

import RwCommon exposing (..)


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


initialModel : Time -> Model
initialModel time = 
  let
    initialElem : Seed -> Elem
    initialElem seed = 
      let
        (color, s1) = ranColor seed
      in
        { x = 0
        , y = 0
        , color = color
        , animX = Nothing 
        , animY = Nothing 
        , seed = s1 }


    seed = initialSeed (round time)
    seeds = createSeedList 100 seed
    elems = List.map initialElem seeds
  in
    { elems = elems}

updateElem : Inp -> Elem -> Elem
updateElem inp elem = 
  let
    updateAnim : Float -> Float -> Maybe Anim -> Seed -> (Maybe Anim, Seed)
    updateAnim span value maybeAnim seed =
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
            maxVal = span / 2 - 50
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
      let 
        easeValue : Time -> Time -> Float -> Float
        easeValue relTime duration to =
          ease easeInExpo Easing.float 0 to duration relTime
        

        animValue : Anim -> Float
        animValue anim = 
          let
            relTime = inp.time - anim.startTime
            diff = easeValue relTime anim.duration anim.to 
          in 
            anim.startVal + diff
    
        
      in
        case maybeAnim of
          Just anim -> animValue anim
          Nothing -> value
        
    
    nextX = updateValue elem.animX elem.x 
    nextY = updateValue elem.animY elem.y
    (nextAnimX, s1) = updateAnim inp.panelSize.w elem.x elem.animX elem.seed
    (nextAnimY, s2) = updateAnim inp.panelSize.h elem.y elem.animY s1
    nextElem = { elem | x = nextX
      , y = nextY
      , animX = nextAnimX
      , animY = nextAnimY
      , seed = s2 }
  in
    nextElem
      

updateModel : Inp -> Maybe Model -> Maybe Model
updateModel inp maybeModel = 
  let
    model = withDefault (initialModel inp.time) maybeModel
    nextElems = List.map (updateElem inp) model.elems
    nextModel = { model | elems = nextElems }
  in 
    Just nextModel