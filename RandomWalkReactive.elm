import RandomWalkModel exposing (..)
import RandomWalkView exposing (..)

import Signal exposing (..)
import Window exposing (..)
import Time exposing (..)
import Graphics.Element exposing (..)



time : Signal Time
time = Time.every (Time.millisecond * 10)


toPanelDim : (Int, Int) -> PanelDim
toPanelDim (x, y) = {
  w = toFloat x
  , h = toFloat y }


panelDim : Signal (Int, Int) -> Signal PanelDim
panelDim dimensions = Signal.map toPanelDim dimensions


panelDimTriggered : Signal PanelDim
panelDimTriggered =
  Signal.sampleOn time (panelDim Window.dimensions)


main : Signal Element
main = Signal.map2
  view
  (panelDim Window.dimensions)
  (Signal.foldp update initial panelDimTriggered)
