module RandomWalkView where

import RandomWalkModel exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)


shape : Shape
shape = circle 100.0


form : Shape -> Form
form shape = shape
  |> filled Color.green
  |> alpha 0.2


toForm : Elem -> Form
toForm elem =
  let
    x = elem.pos.x
    y = elem.pos.y
  in
    form shape
      |> move (x, y)


view : PanelDim -> Model -> Element
view panel model =
  let
    w = round panel.w
    h = round panel.h
  in
    collage w h (List.map toForm model.elems)
