import RandomWalkModel exposing (..)
import RandomWalkView exposing (..)

import Signal exposing (..)
import Window exposing (..)
import Time exposing (..)
import Graphics.Element exposing (..)
import Maybe exposing (..)


timeSig : Signal Time
timeSig = Time.every (Time.millisecond)


toPanelDim : (Int, Int) -> PanelDim
toPanelDim (x, y) = {
  w = toFloat x
  , h = toFloat y }


panelDimSig : Signal PanelDim
panelDimSig =
  Signal.map toPanelDim Window.dimensions


inp : Time -> PanelDim -> Inp
inp time panelDim =
  { time = time
  , panelDim = panelDim }


inpSig : Signal Inp
inpSig =
  Signal.map2 inp timeSig panelDimSig


leftSig : Signal (Maybe (left, right)) -> Signal (Maybe left)
leftSig sig =
  let
    left (l, r) = l
    leftSig1 tuple = Maybe.map left tuple
  in
    Signal.map leftSig1 sig


main : Signal Element
main =
  let
    modelSig = Signal.foldp update Nothing inpSig
  in
    Signal.map2 view panelDimSig (leftSig modelSig)
