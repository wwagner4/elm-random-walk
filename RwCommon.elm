module RwCommon where

import Random exposing (..)

ranBool : Float -> Seed -> (Bool, Seed)
ranBool probabillity seed =
  let
    (val, nextSeed) = generate (Random.float 0 1) seed
    bool = val <= probabillity 
  in
    (bool, nextSeed)


