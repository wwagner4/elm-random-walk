module RandomWalkModel where

import Random exposing (..)


type alias PanelDim =
  { w : Float
  , h : Float }

type alias Pos =
  { x: Float
  , y : Float }

type alias Elem = { pos : Pos }


type alias Model =
  { seed : Seed
  , elems : List Elem }


initialPos : Pos
initialPos = { x = 0.0, y = 0.0 }


initialElem : Elem
initialElem = { pos = initialPos }


initial : Model
initial =
  { seed = initialSeed 821736182376
  , elems = List.repeat 100 initialElem }


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


update : PanelDim -> Model -> Model
update panelDim model =
  let
    (panelDim, nextSeed, nextElems) =
      List.foldl
        updateFoldElem
        (panelDim, model.seed, [])
        model.elems
  in
    { model |
      elems = nextElems
      , seed = nextSeed }
