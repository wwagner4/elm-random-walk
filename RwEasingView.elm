module RwEasingView where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Time exposing (..)
import Maybe exposing (..)
import Color exposing (..)

import RwEasingModel exposing (..)

elemToForm : Elem -> Form
elemToForm elem =
  rect 100 200
    |> filled elem.color
    |> alpha 0.5
    |> move (elem.x, elem.y)
  
  
modelToForm : Model -> List Form
modelToForm model =
  List.map elemToForm model.elems
  
  
toForms : Maybe Model -> List Form
toForms maybeModel = case maybeModel of
  Just model -> modelToForm model
  Nothing -> []


viewModel : (Int, Int) -> Maybe Model -> Element
viewModel (width, height) maybeModel =
  collage width height (toForms maybeModel)



