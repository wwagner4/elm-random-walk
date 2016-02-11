module RwCommon where

import Random exposing (..)
import Color exposing (..)


{-| Creates a list of seeds
-}
createSeedList : Int -> Seed -> List Seed
createSeedList count seed =
  if count == 0 then []
  else 
    let 
      (i, nextSeed) = generate (Random.int minInt maxInt) seed
      rest = createSeedList (count - 1) nextSeed 
    in
      nextSeed :: rest


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
    (diff, nextSeed)
  
ranPositiveFloat : Float -> Seed -> (Float, Seed)
ranPositiveFloat maxVal seed =
  let
    gen = Random.float 0 maxVal
    (diff, nextSeed) = generate gen seed
  in
    (diff, nextSeed)
  
ranInt : Int -> Seed -> (Int, Seed)
ranInt maxVal seed =
  let
    gen = Random.int -maxVal maxVal
    (diff, nextSeed) = generate gen seed
  in
    (diff, nextSeed)
  
    
ranColor : Seed -> (Color, Seed)
ranColor seed =
  let
    gen = Random.float 0 360
    (ranDeg, nextSeed) = generate gen seed
    col = hsl (degrees ranDeg) 1 0.5
  in
    (col, nextSeed)
      
        
ranColorCompl : Float -> Seed -> (Color, Seed)
ranColorCompl hueOffset seed =
  let
    col1 = hsl (degrees hueOffset) 1 0.5
    col2 = hsl (degrees (hueOffset + 120)) 1 0.5
    col3 = hsl (degrees (hueOffset + 240)) 1 0.5

    gen = Random.int 0 2
    (index, nextSeed) = generate gen seed
    col = 
      if index == 0 then col1
      else if index == 1 then col2
      else col3
  in
    (col, nextSeed)
      
        
ranColorRgb : Seed -> (Color, Seed)
ranColorRgb seed =
  let
    gen = Random.int 0 2
    (index, nextSeed) = generate gen seed
    col = 
      if index == 0 then Color.red
      else if index == 1 then Color.green
      else Color.blue
  in
    (col, nextSeed)
      
        


