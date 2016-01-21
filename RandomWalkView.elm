module RandomWalkView where

import RandomWalkModel exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Maybe exposing (..)


shape : Shape
shape = square 15.0


form : Shape -> Elem -> Form
form shape elem = shape
  |> filled elem.color
  |> alpha 0.9


toForm : Elem -> Form
toForm elem =
  let
    x = elem.pos.x
    y = elem.pos.y
  in
    form shape elem
      |> move (x, y)


toForms : (Model, any) -> List Form
toForms (model, seed) =
  List.map toForm model.elems


-- TODO Remove the any param that is not needed
view : PanelDim -> Maybe (Model, any) -> Element
view panel maybeModel =
  let
    w = round panel.w
    h = round panel.h
    maybeForms = Maybe.map toForms maybeModel
    forms = withDefault [] maybeForms
  in
    collage w h forms
