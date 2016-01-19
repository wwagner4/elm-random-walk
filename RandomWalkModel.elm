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
  , elems = List.repeat 100 initialElem }


diffVal : Float
diffVal = 5.0


diffGen : Generator Float
diffGen = Random.float -diffVal diffVal


ranDiff : Seed -> (Float, Seed)
ranDiff seed = generate diffGen seed

updateX : PanelDim -> Seed -> Pos -> (Float, Seed)
updateX panel seed pos =
  let
    (diff, nextSeed) = ranDiff seed
    max = panel.w / 2.0
    corr1 = if (pos.x > max * 0.9) then -diffVal else 0
    corr2 = if (pos.x < -max * 0.9) then diffVal else 0
    nextX = pos.x + diff + corr1 + corr2
  in
    (nextX, nextSeed)


updateY : PanelDim -> Seed -> Pos -> (Float, Seed)
updateY panel seed pos =
  let
    (diff, nextSeed) = ranDiff seed
    max = panel.h / 2.0
    corr1 = if (pos.y > max * 0.9) then -diffVal else 0
    corr2 = if (pos.y < -max * 0.9) then diffVal else 0
    nextY = pos.y + diff + corr1 + corr2
  in
    (nextY, nextSeed)


updatePos : PanelDim -> Seed -> Pos -> (Pos, Seed)
updatePos panel seed pos =
  let
    (nextX, s1) = updateX panel seed pos
    (nextY, s2) = updateY panel s1 pos
    nextPos = { pos | x = nextX , y = nextY }
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
