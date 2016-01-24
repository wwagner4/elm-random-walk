module RwEasingView where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Time exposing (..)
import Maybe exposing (..)
import Color exposing (..)

import RwEasingModel exposing (..)

viewTime : Time -> Element
viewTime time = show time


viewAnim : Time -> Element
viewAnim time =
  show (time, (anim time))
  
  
toForm : Model -> Form
toForm model =
  circle 50
    |> filled Color.green
    |> alpha 0.8
    |> move (model.x, model.y)
  
  
toForms : Maybe Model -> List Form
toForms maybeModel = case maybeModel of
  Just model -> [toForm model]
  Nothing -> []


viewModel : (Int, Int) -> Maybe Model -> Element
viewModel (width, height) maybeModel =
  collage width height (toForms maybeModel)



