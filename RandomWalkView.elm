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

toForms1 : Model -> List Form
toForms1 model =
  List.map toForm model.elems



transform : Maybe (a, b) -> Maybe a
transform tupl =
  Maybe.map (\(a, b) -> a) tupl


view : PanelDim -> Maybe (Model, any) -> Element
view panel maybeModel =
  let
    w = round panel.w
    h = round panel.h
    maybeForms = Maybe.map toForms maybeModel
    forms = withDefault [] maybeForms
  in
    collage w h forms

view1 : PanelDim -> Maybe Model -> Element
view1 panel maybeModel =
  let
    w = round panel.w
    h = round panel.h
    maybeForms = Maybe.map toForms1 maybeModel
    forms = withDefault [] maybeForms
  in
    collage w h forms
