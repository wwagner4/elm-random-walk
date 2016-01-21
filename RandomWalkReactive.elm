import RandomWalkModel exposing (..)
import RandomWalkView exposing (..)

import Signal exposing (..)
import Window exposing (..)
import Time exposing (..)
import Graphics.Element exposing (..)
import Maybe exposing (..)
import Random exposing (..) -- TODO random should not be used here


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



modelSig : Signal (Maybe (Model, Seed))
modelSig =
  Signal.foldp update Nothing inpSig


main : Signal Element
main =
  Signal.map2 view panelDimSig modelSig
