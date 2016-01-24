module RwEasingModel where

import Time exposing (..)
import Easing exposing (..)
import Signal exposing (..)
import Maybe exposing (..)
import Color exposing (..)



type alias Model =
  { x : Float
  , y : Float
  , anim : Maybe Anim }
  
  
type alias Anim =
  { startVal : Float
  , startTime : Time }


initial : Model
initial = 
  { x = 0
  , y = 0
  , anim = Nothing }


animEaseValue : Time -> Float
animEaseValue relTime =
    ease easeOutElastic float 0 400 (second * 5) relTime


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
    model = withDefault initial maybeModel
    nextModel = case model.anim of
      Just anim -> { model | 
        x = animValue time anim }
      Nothing -> { model | 
        anim = Just {startVal = model.x, startTime = time} }
  in 
    Just nextModel