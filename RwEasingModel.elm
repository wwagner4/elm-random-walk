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





anim : Time -> Float
anim currentTime =
    ease easeOutBack float 0 500 second currentTime


