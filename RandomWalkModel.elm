module RandomWalkModel where

import Random exposing (..)


type alias PanelDim = {
  w : Float
  , h : Float
}

type alias Pos = {
  x: Float
  , y : Float
}

type alias Elem = {
  pos : Pos
}

type alias Model = {
  seed : Seed
  , elems : List Elem
}

initialPos : Pos
initialPos = { x = 0.0, y = 0.0 }


initialElem : Elem
initialElem = { pos = initialPos }


initial : Model
initial = {
  seed = initialSeed 821736182376
  , elems = List.repeat 20 initialElem }


diffVal : Float
diffVal = 20.0


diffGen : Generator Float
diffGen = Random.float -diffVal diffVal


ranDiff : Seed -> (Float, Seed)
ranDiff seed = generate diffGen seed


updateValue : Float -> Float -> Seed -> Float -> (Float, Seed)
updateValue min max seed value =
  let
    (diff, nextSeed) = ranDiff seed
    maxCorr = if value > max then (-diffVal / 100) else 0
    minCorr = if value < min then (diffVal / 100) else 0
    next = value + diff + maxCorr + minCorr
  in
    (next, nextSeed)


updatePos : PanelDim -> Seed -> Pos -> (Pos, Seed)
updatePos panel seed pos =
  let
    border = 0
    maxX = panel.w - border
    minX = -panel.w + border
    maxY = panel.h - border
    minY = -panel.h + border
    (nextX, s1) = updateValue minX maxX seed pos.x
    (nextY, nextSeed) = updateValue minY maxY s1 pos.y
    nextPos = { pos | x = nextX , y = nextY }
  in
    (nextPos, nextSeed)


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
