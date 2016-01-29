module RwCommon where

import Random exposing (..)

ranBool : Seed -> (Bool, Seed)
ranBool seed =
  let
    (int, nextSeed) = generate (Random.int 1 1000) seed
    bool = int < 10
  in
    (bool, nextSeed)


