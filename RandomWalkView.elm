module RandomWalkView where

import RandomWalkModel exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Maybe exposing (..)


shape : Shape
shape = circle 100.0


form : Shape -> Elem -> Form
form shape elem = shape
  |> filled elem.color
  |> alpha 0.1


toForm : Elem -> Form
toForm elem =
  let
    x = elem.pos.x
    y = elem.pos.y
  in
    form shape elem
      |> move (x, y)


view : PanelDim -> Maybe Model -> Element
view panel maybeModel =
  let
    w = round panel.w
    h = round panel.h
    model = withDefault emptyModel maybeModel
  in
    collage w h (List.map toForm model.elems)
