module RwEasingModel where

import RwCommon exposing (..)

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


initialModel : Time -> Model
initialModel time = 
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
    seeds = createSeedList 30 seed
    elems = List.map initialElem seeds
  in
    { elems = elems}

    
updateAnimElem : Time -> Anim -> Elem -> Elem
updateAnimElem time anim elem = 
  let
    animEaseValue : Time -> Time -> Float -> Float
    animEaseValue relTime duration to =
      let
        from = 0
      in
        ease easeOutCubic Easing.float from to duration relTime


    animValue : Time -> Anim -> Float
    animValue time anim= 
      let
        relTime = time - anim.startTime
        diff = animEaseValue relTime anim.duration anim.to 
      in 
        anim.startVal + diff
    
        
    animReady = (time - anim.startTime) > anim.duration
    nextAnim = 
      if (animReady) then { elem | x = animValue time anim , anim = Nothing }
      else { elem | x = animValue time anim }
  in
    nextAnim
  

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

updateElem : Inp -> Elem -> Elem
updateElem inp elem = 
  case elem.anim of
    Just anim -> updateAnimElem inp.time anim elem
    Nothing -> updateNoAnimElem inp elem


updateModel : Inp -> Maybe Model -> Maybe Model
updateModel inp maybeModel = 
  let
    model = withDefault (initialModel inp.time) maybeModel
    nextElems = List.map (updateElem inp) model.elems
    nextModel =
      { model | elems = nextElems }
  in 
    Just nextModel