module RwEasingModel where

import Time exposing (..)
import Easing exposing (..)
import Signal exposing (..)
import Maybe exposing (..)
import Color exposing (..)



type alias Model =
  { x : Float
  , y : Float
  , startTime : Time }


initial : Model
initial = 
  { x = 0
  , y = 0
  , startTime = 0 }


anim : Time -> Float
anim currentTime =
    ease easeOutBack float 0 500 second currentTime


updateModel : Time -> Model -> Model
updateModel time model = model