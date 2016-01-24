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
    ease easeOutElastic Easing.float 0 400 (second * 5) relTime


animValue : Time -> Anim -> Float
animValue time anim= 
  let
    relTime = time - anim.startTime
    diff = animEaseValue relTime 
  in 
    anim.startVal + diff
    

updateModel : Time -> Maybe Model -> Maybe Model
updateModel time maybeModel = 
  let
    model = withDefault (initial time) maybeModel
    nextModel = case model.anim of
      Just anim -> { model | 
        x = animValue time anim }
      Nothing -> { model | 
        anim = Just {startVal = model.x, startTime = time} }
  in 
    Just nextModel