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
            updateVal : Seed -> Float -> (Float, Seed)
            updateVal seed val =
              let
                (diff, nextSeed) = ranFloat 10 seed
                nextVal = val + diff
              in
                (nextVal, nextSeed)
            
            
            adjustVal : Float -> Float -> Float
            adjustVal span val =
              let
                maxVal = span / 2.0
                minVal = -maxVal
                r1 = min val maxVal
                r2 = max r1 minVal
              in
                r2
        
        
            border = 50
            panel = inp.panelDim
            (nextX, s1) = updateVal seed pos.x
            (nextY, s2) = updateVal s1 pos.y
            adjX = adjustVal (panel.w - border * 2) nextX
            adjY = adjustVal (panel.h - border * 2) nextY
            nextPos = { pos | x = adjX , y = adjY }
          in
            (nextPos, s2)
    
    
        (doMove, s1) = ranBool 0.02 seed
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
        initialElem : Seed -> (Elem, Seed)
        initialElem seed =
          let
            initialPos : Pos
            initialPos = { x = 0.0, y = 0.0 }
        
        
            (col, nextSeed) = ranColorCompl seed 200
            elem =
              { pos = initialPos
              , color = col }
          in
            (elem, nextSeed)
    
    
        initialElems : Int -> Seed -> (List Elem, Seed)
        initialElems cnt seed =
          if cnt == 0 then ([], seed)
          else
            let
              (elem, s1) = initialElem seed
              (restElems, s2) = initialElems (cnt - 1) s1
              elems = elem :: restElems
            in
              (elems, s2)
    
    
        s1 = initialSeed (round startTime)
        (elems, s2) = initialElems 400 s1
        model =
          { elems = elems }
        in
          (model, s2)


    (model, seed) = withDefault (initial inp.time) maybeModel
    (nextElems, nextSeed) =
      List.foldr updateElems ([], seed) model.elems
    nextModel = { model | elems = nextElems }
  in
    Just (nextModel, nextSeed)
