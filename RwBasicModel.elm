module RwBasicModel where

import Random exposing (..)
import Color exposing (..)
import Time exposing (..)
import Maybe exposing (..)


type alias PanelDim =
  { w : Float
  , h : Float }

type alias Pos =
  { x: Float
  , y : Float }

type alias Elem =
  { pos : Pos
  , color : Color }


type alias Model =
  { elems : List Elem }


type alias Inp =
  { time : Time
  , panelDim : PanelDim }


inp : Time -> PanelDim -> Inp
inp time panelDim =
  { time = time
  , panelDim = panelDim }


initialPos : Pos
initialPos = { x = 0.0, y = 0.0 }


ranColor : Seed -> (Color, Seed)
ranColor seed =
  let
    gen = Random.float 0 360
    (ranDeg, nextSeed) = generate gen seed
    col = hsl (degrees ranDeg) 1 0.5
  in
    (col, nextSeed)


initialElem : Seed -> (Elem, Seed)
initialElem seed =
  let
    (col, nextSeed) = ranColor seed
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


initial : Time -> (Model, Seed)
initial startTime =
  let
    s1 = initialSeed (round startTime)
    (elems, s2) = initialElems 40 s1
    model =
      { elems = elems }
    in
      (model, s2)


ranDiff : Seed -> (Float, Seed)
ranDiff seed =
  let
    diffVal = 2.0
    gen = Random.float -diffVal diffVal
    (diff, nextSeed) = generate gen seed
  in
    (diff * 10, nextSeed)


ranBool : Seed -> (Bool, Seed)
ranBool seed =
  let
    (int, nextSeed) = generate (Random.int 1 1000) seed
    bool = int < 10
  in
    (bool, nextSeed)


updateVal : Seed -> Float -> (Float, Seed)
updateVal seed val =
  let
    (diff, nextSeed) = ranDiff seed
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


updatePos : PanelDim -> Seed -> Pos -> (Pos, Seed)
updatePos panel seed pos =
  let
    border = 50
    (nextX, s1) = updateVal seed pos.x
    (nextY, s2) = updateVal s1 pos.y
    adjX = adjustVal (panel.w - border * 2) nextX
    adjY = adjustVal (panel.h - border * 2) nextY
    nextPos = { pos | x = adjX , y = adjY }
  in
    (nextPos, s2)


updateElem : PanelDim -> Seed -> Elem -> (Elem, Seed)
updateElem panel seed elem =
  let
    (bool, s1) = ranBool seed
    (nextPos, s2) = updatePos panel s1 elem.pos
    nextElem =
      if bool then { elem | pos = nextPos }
      else elem
  in
    (nextElem, s2)


updateFoldElem : Elem -> (PanelDim, Seed, List Elem) -> (PanelDim, Seed, List Elem)
updateFoldElem elem (panelDim, seed, elems) =
  let
    (nextElem, nextSeed) = updateElem panelDim seed elem
    nextElems = nextElem :: elems
  in
    (panelDim, nextSeed, nextElems)


update : Inp -> Maybe (Model, Seed) -> Maybe (Model, Seed)
update inp maybeModel =
  let

    (model, seed) = withDefault (initial inp.time) maybeModel

    (panelDim, nextSeed, nextElems) =
      List.foldr
        updateFoldElem
        (inp.panelDim, seed, [])
        model.elems

    nextModel = { model | elems = nextElems }

  in
    Just (nextModel, nextSeed)
