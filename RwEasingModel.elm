module RwEasingModel where

import Time exposing (..)
import Easing exposing (..)
import Signal exposing (..)
import Maybe exposing (..)
import Color exposing (..)



type alias Model =
  { x : Float
  , y : Float
  , startX : Float
  , startTime : Time }


initial : Time -> Model
initial startTime = 
  { x = -200
  , y = 0
  , startX = -200
  , startTime = startTime }


anim : Time -> Float
anim currentTime =
    ease easeOutBounce float 0 400 (second * 2) currentTime


updateModel : Time -> Maybe Model -> Maybe Model
updateModel time maybeModel = 
  let
    model = withDefault (initial time) maybeModel
    relTime = time - model.startTime
    diff = anim relTime 
    x = model.startX + diff
    nextModel = { model | 
      x = model.startX + diff }
  in 
    Just nextModel