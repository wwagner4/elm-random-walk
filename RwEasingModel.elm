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


initial : Model
initial = 
  { x = -200
  , y = 0
  , startX = -200
  , startTime = 0 }


anim : Time -> Float
anim currentTime =
    ease easeOutBounce float 0 400 (millisecond * 500) currentTime


updateModel : Time -> Model -> Model
updateModel time model = 
  let
    relTime = time - model.startTime
    diff = anim relTime 
    x = model.startX + diff
  in 
    { model | 
      x = model.startX + diff }