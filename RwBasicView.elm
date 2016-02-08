module RwBasicView where

import RwBasicModel exposing (..)

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Maybe exposing (..)


toForm : Elem -> Form
toForm elem =
  square 60.0
    |> filled elem.color
    |> alpha 0.3
    |> move (elem.pos.x, elem.pos.y)


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
