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


transform : Maybe (a, b) -> Maybe a
transform tupl =
  Maybe.map (\(a, b) -> a) tupl


transformModel : Signal (Maybe (a, b)) -> Signal (Maybe a)
transformModel c =
  Signal.map transform c


main : Signal Element
main =
  let
    modelSig = Signal.foldp update Nothing inpSig
  in
    Signal.map2 view1 panelDimSig (transformModel modelSig)
