module RwCommon where

import Random exposing (..)

ranBool : Float -> Seed -> (Bool, Seed)
ranBool probabillity seed =
  let
    (val, nextSeed) = generate (Random.float 0 1) seed
    bool = val <= probabillity 
  in
    (bool, nextSeed)


ranDiff : Seed -> (Float, Seed)
ranDiff seed =
  let
    diffVal = 10.0
    gen = Random.float -diffVal diffVal
    (diff, nextSeed) = generate gen seed
  in
    (diff * 10, nextSeed)
  
    


