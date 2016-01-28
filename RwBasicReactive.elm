module RwBasicReactive where

import RwBasicModel exposing (..)
import RwBasicView exposing (..)

import Graphics.Element exposing (..)
import Signal exposing (..)
import Window exposing (..)
import Time exposing (..)
import Maybe exposing (..)
import Random exposing (..)


timeSig : Signal Time
timeSig =
  Time.every (Time.second * 0.05)


panelDimSig : Signal PanelDim
panelDimSig =
  let
    toPanelDim (x, y) = {
      w = toFloat x
      , h = toFloat y }
  in
    Signal.map toPanelDim Window.dimensions


inpSig : Signal Inp
inpSig =
  Signal.map2 inp timeSig panelDimSig


leftMaybeSig : Signal (Maybe (left, right)) -> Signal (Maybe left)
leftMaybeSig sig =
  Signal.map (Maybe.map (\(l, r) -> l)) sig


modelSig : Signal (Maybe (Model, Seed))
modelSig =
  Signal.foldp update Nothing inpSig


main : Signal Element
main =
  Signal.map2 view panelDimSig (leftMaybeSig modelSig)
