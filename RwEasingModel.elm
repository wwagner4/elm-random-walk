module RwEasingModel where

import Time exposing (..)
import Easing exposing (..)
import Signal exposing (..)
import Maybe exposing (..)
import Color exposing (..)
import Random exposing (..)


type alias Model =
  { x : Float
  , y : Float
  , anim : Maybe Anim 
  , seed : Seed }
  
  
type alias Anim =
  { startVal : Float
  , startTime : Time 
  , duration : Time 
  , to : Float }
  
  
type alias PanelSize = 
  { w : Float
  , h : Float }


type alias Inp = 
  { time : Time
  , panelSize : PanelSize }


initial : Time -> Model
initial time = 
  { x = 0
  , y = 0
  , anim = Nothing 
  , seed = initialSeed (round time) }


animEaseValue : Time -> Time -> Float -> Float
animEaseValue relTime duration to =
  let
    from = 0
  in
    ease easeInOutCirc Easing.float from to duration relTime


animValue : Time -> Anim -> Float
animValue time anim= 
  let
    relTime = time - anim.startTime
    diff = animEaseValue relTime anim.duration anim.to 
  in 
    anim.startVal + diff
    
        
updateAnimModel : Time -> Anim -> Model -> Model
updateAnimModel time anim model = 
  let
    animReady = (time - anim.startTime) > anim.duration
  in
    if (animReady) then
      { model | x = animValue time anim 
        , anim = Nothing }
    else
      { model | x = animValue time anim }
  

updateNoAnimModel : Inp -> Model -> Model
updateNoAnimModel inp model = 
  let 
    maxX = inp.panelSize.w / 2
    minX = -maxX
    gen = 
      if (model.x > maxX) then Random.float -500 0
      else if (model.x < minX) then Random.float 0 500
      else Random.float -500 500
    (to, nextSeed) = generate gen model.seed
    newAnim = 
      { startVal = model.x 
      , startTime = inp.time
      , duration = second * 3 
      , to = to}
  in
    { model | seed = nextSeed
      , anim = Just newAnim }


updateModel : Inp -> Maybe Model -> Maybe Model
updateModel inp maybeModel = 
  let
    model = withDefault (initial inp.time) maybeModel
    nextModel = case model.anim of
      Just anim -> updateAnimModel inp.time anim model
      Nothing -> updateNoAnimModel inp model
  in 
    Just nextModel