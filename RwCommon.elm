module RwCommon where

import Random exposing (..)

ranBool : Float -> Seed -> (Bool, Seed)
ranBool probabillity seed =
  let
    (val, nextSeed) = generate (Random.float 0 1) seed
    bool = val <= probabillity 
  in
    (bool, nextSeed)


ranFloat : Float -> Seed -> (Float, Seed)
ranFloat maxVal seed =
  let
    gen = Random.float -maxVal maxVal
    (diff, nextSeed) = generate gen seed
  in
    (diff * 10, nextSeed)
  
    


