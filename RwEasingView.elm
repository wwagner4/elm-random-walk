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


viewModel : (Int, Int) -> Model -> Element
viewModel (width, height) model =
  let
    shape = circle 20
    form = shape
      |> filled Color.red
      |> alpha 0.4
      |> move (model.x, model.y)
  in
    collage width height [form]



