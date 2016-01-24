module RwEasingView where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Time exposing (..)
import Maybe exposing (..)
import Color exposing (..)

import RwEasingModel exposing (..)

showTime : Time -> Element
showTime time = show time


showAnim : Time -> Element
showAnim time =
  show (time, (anim time))


showModel : Model -> Element
showModel model =
  let
    shape = circle 20
    form = shape
      |> filled Color.red
      |> alpha 0.4
      |> move (model.x, model.y)
  in
    collage 500 500 [form]



