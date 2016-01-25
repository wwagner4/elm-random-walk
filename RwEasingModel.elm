module RwEasingModel where

import Time exposing (..)
import Easing exposing (..)
import Signal exposing (..)
import Maybe exposing (..)
import Color exposing (..)
import Random exposing (..)


type alias Model =
  { x : Float
  , y : Float
  , anim : Maybe Anim 
  , seed : Seed }
  
  
type alias Anim =
  { startVal : Float
  , startTime : Time 
  , duration : Time }
  
  
type alias PanelSize = 
  { w : Float
  , h : Float }


type alias Inp = 
  { time : Time
  , panelSize : PanelSize }


initial : Time -> Model
initial time = 
  { x = 0
  , y = 0
  , anim = Nothing 
  , seed = initialSeed (round time) }


animEaseValue : Time -> Time -> Float
animEaseValue relTime duration =
  let
    from = 0
    to = 100
  in
    ease easeOutElastic Easing.float from to duration relTime


animValue : Time -> Anim -> Float
animValue time anim= 
  let
    relTime = time - anim.startTime
    diff = animEaseValue relTime anim.duration 
  in 
    anim.startVal + diff
    
    
updateAnimModel : Time -> Anim -> Model -> Model
updateAnimModel time anim model = 
  { model | 
    x = animValue time anim }
  

updateNoAnimModel : Time -> Model -> Model
updateNoAnimModel time model = 
  let 
    newAnim = 
      { startVal = model.x 
      , startTime = time
      , duration = second * 10 }
  in
    { model | 
      anim = Just newAnim }


updateModel : Time -> Maybe Model -> Maybe Model
updateModel time maybeModel = 
  let
    model = withDefault (initial time) maybeModel
    nextModel = case model.anim of
      Just anim -> updateAnimModel time anim model
      Nothing -> updateNoAnimModel time model
  in 
    Just nextModel