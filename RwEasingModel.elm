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
  , seed : Seed}
  
  
type alias Anim =
  { startVal : Float
  , startTime : Time }


initial : Time -> Model
initial time = 
  { x = 0
  , y = 0
  , anim = Nothing 
  , seed = initialSeed (round time) }


animEaseValue : Time -> Float
animEaseValue relTime =
  let
    duration = second * 5
    from = 0
    to = 300
  in
    ease easeOutElastic Easing.float from to duration relTime


animValue : Time -> Anim -> Float
animValue time anim= 
  let
    relTime = time - anim.startTime
    diff = animEaseValue relTime 
  in 
    anim.startVal + diff
    
    
updateAnimModel : Time -> Anim -> Model -> Model
updateAnimModel time anim model = 
  { model | 
    x = animValue time anim }
  

updateNoAnimModel : Time -> Model -> Model
updateNoAnimModel time model = 
  { model | 
    anim = Just {startVal = model.x, startTime = time} }


updateModel : Time -> Maybe Model -> Maybe Model
updateModel time maybeModel = 
  let
    model = withDefault (initial time) maybeModel
    nextModel = case model.anim of
      Just anim -> updateAnimModel time anim model
      Nothing -> updateNoAnimModel time model
  in 
    Just nextModel