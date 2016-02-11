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
    updateElem : Elem -> Seed -> (Elem, Seed)
    updateElem elem seed =
      let
        updatePos : Pos -> Seed -> (Pos, Seed)
        updatePos pos seed =
          let
            updateVal : Float -> Float -> Seed -> (Float, Seed)
            updateVal span val seed =
              let
                maxVal = span / 2.0
                minVal = -maxVal
                (diff, s1) = ranInt 1 seed
                (doMove, s2) = ranBool 0.1 s1 
                nextVal = 
                  if doMove then val + (toFloat diff) * 20.0
                  else val
                adjVal = 
                  if nextVal > maxVal then val 
                  else if nextVal < minVal then val
                  else nextVal
              in
                (adjVal, s2)
        
        
            border = -150
            panel = inp.panelDim
            (nextX, s1) = updateVal (panel.w - border * 2) pos.x seed
            (nextY, s2) = updateVal (panel.h - border * 2)pos.y s1
            nextPos = { pos | x = nextX , y = nextY }
          in
            (nextPos, s2)
    
    
        (nextPos, s1) = updatePos elem.pos seed
        nextElem = { elem | pos = nextPos }
      in
        (nextElem, s1)        

    updateElems : Elem -> (List Elem, Seed) -> (List Elem, Seed)
    updateElems elem (elems, seed) =
      let
        (nextElem, nextSeed) = updateElem elem seed
        nextElems = nextElem :: elems
      in
        (nextElems, nextSeed)


    initial : (Model, Seed)
    initial =
      let
        initialElem : Float -> Seed -> (Elem, Seed)
        initialElem colorOff seed =
          let
            initialPos : Pos
            initialPos = { x = 0.0, y = 0.0 }
        
        
            (col, nextSeed) = ranColorCompl colorOff seed 
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
    
    
        s1 = initialSeed (round inp.time)
        (colorOff, s2) = ranPositiveFloat 360 s1
        (newElems, s3) = initialElems 2000 colorOff s2
        nextModel = { elems = newElems }
      in
        (nextModel, s3)


    (model, seed) = withDefault initial maybeModel
    (nextElems, nextSeed) =
      List.foldr updateElems ([], seed) model.elems
    nextModel = { model | elems = nextElems }
  in
    Just (nextModel, nextSeed)
