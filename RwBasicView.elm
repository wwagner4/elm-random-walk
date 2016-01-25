module RwBasicView where

import RwBasicModel exposing (..)

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Maybe exposing (..)


shape : Shape
shape = square 80.0


form : Shape -> Elem -> Form
form shape elem = shape
  |> filled elem.color
  |> alpha 0.3


toForm : Elem -> Form
toForm elem =
  let
    x = elem.pos.x
    y = elem.pos.y
  in
    form shape elem
      |> move (x, y)


toForms : Model -> List Form
toForms model =
  List.map toForm model.elems


view : PanelDim -> Maybe Model -> Element
view panel maybeModel =
  let
    w = round panel.w
    h = round panel.h
    maybeForms = Maybe.map toForms maybeModel
    forms = withDefault [] maybeForms
  in
    collage w h forms
