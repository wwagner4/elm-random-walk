module RandomWalkModel where

import Random exposing (..)
import Color exposing (..)
import Time exposing (..)


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
  { seed : Seed
  , elems : List Elem }


type alias Inp =
  { time : Time
  , panelDim : PanelDim }


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


initial : Time -> Model
initial startTime =
  let
    seed = initialSeed (round startTime)
    (elem, nextSeed) = initialElem seed
  in
    { seed = nextSeed
    , elems = List.repeat 100 elem }


ranDiff : Seed -> (Float, Seed)
ranDiff seed =
  let
    diffVal = 5.0
    gen = Random.float -diffVal diffVal
  in
    generate gen seed


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
    (nextPos, nextSeed) = updatePos panel seed elem.pos
    nextElem = { elem | pos = nextPos }
  in
    (nextElem, nextSeed)


updateFoldElem : Elem -> (PanelDim, Seed, List Elem) -> (PanelDim, Seed, List Elem)
updateFoldElem elem (panelDim, seed, elems) =
  let
    (nextElem, nextSeed) = updateElem panelDim seed elem
    nextElems = nextElem :: elems
  in
    (panelDim, nextSeed, nextElems)


update : Inp -> Model -> Model
update inp model =
  let
    (panelDim, nextSeed, nextElems) =
      List.foldl
        updateFoldElem
        (inp.panelDim, model.seed, [])
        model.elems
  in
    { model |
      elems = nextElems
      , seed = nextSeed }
