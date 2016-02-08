module RwBasicModel where

import RwCommon exposing (..)

import Random exposing (..)
import Color exposing (..)
import Time exposing (..)
import Maybe exposing (..)


type alias Pos =
  { x: Float
  , y : Float }
  

type alias Elem =
  { pos : Pos
  , color : Color }


type alias Model =
  { elems : List Elem }


type alias PanelDim =
  { w : Float
  , h : Float }


panelDim : Float -> Float -> PanelDim
panelDim w h = 
  { w = w
  , h = h }


type alias Inp =
  { time : Time
  , panelDim : PanelDim }


inp : Time -> PanelDim -> Inp
inp time panelDim =
  { time = time
  , panelDim = panelDim }


update : Inp -> Maybe (Model, Seed) -> Maybe (Model, Seed)
update inp maybeModel =
  let
    updateElem : Seed -> Elem -> (Elem, Seed)
    updateElem seed elem =
      let
        updatePos : Seed -> Pos -> (Pos, Seed)
        updatePos seed pos =
          let
            updateVal : Seed -> Float -> Float -> (Float, Seed)
            updateVal seed span val =
              let
                maxVal = span / 2.0
                minVal = -maxVal
                (diff, nextSeed) = ranInt 1 seed
                nextVal = val + (toFloat diff) * 20.0
                adjVal = 
                  if nextVal > maxVal then val 
                  else if nextVal < minVal then val
                  else nextVal
              in
                (adjVal, nextSeed)
        
        
            border = -150
            panel = inp.panelDim
            (nextX, s1) = updateVal seed (panel.w - border * 2) pos.x
            (nextY, s2) = updateVal s1 (panel.h - border * 2)pos.y
            nextPos = { pos | x = nextX , y = nextY }
          in
            (nextPos, s2)
    
    
        (doMove, s1) = ranBool 0.1 seed
        elemTupl = 
          if doMove then 
            let
              (nextPos, s2) = updatePos s1 elem.pos
            in
              ({ elem | pos = nextPos }, s2)
          else 
            (elem, s1)
      in
        elemTupl        

    updateElems : Elem -> (List Elem, Seed) -> (List Elem, Seed)
    updateElems elem (elems, seed) =
      let
        (nextElem, nextSeed) = updateElem seed elem
        nextElems = nextElem :: elems
      in
        (nextElems, nextSeed)


    initial : Time -> (Model, Seed)
    initial startTime =
      let
        initialElem : Float -> Seed -> (Elem, Seed)
        initialElem colorOff seed =
          let
            initialPos : Pos
            initialPos = { x = 0.0, y = 0.0 }
        
        
            (col, nextSeed) = ranColorCompl seed colorOff
            elem =
              { pos = initialPos
              , color = col }
          in
            (elem, nextSeed)
    
    
        initialElems : Int -> Float -> Seed -> (List Elem, Seed)
        initialElems cnt colorOff seed =
          if cnt == 0 then ([], seed)
          else
            let
              (elem, s1) = initialElem colorOff seed
              (restElems, s2) = initialElems (cnt - 1) colorOff s1
              elems = elem :: restElems
            in
              (elems, s2)
    
    
        s1 = initialSeed (round startTime)
        (colorOff, s2) = ranPositiveFloat 300 s1
        (elems, s3) = initialElems 400 colorOff s2
        nextModel = { elems = elems }
      in
        (nextModel, s3)


    (model, seed) = withDefault (initial inp.time) maybeModel
    (nextElems, nextSeed) =
      List.foldr updateElems ([], seed) model.elems
    nextModel = { model | elems = nextElems }
  in
    Just (nextModel, nextSeed)
