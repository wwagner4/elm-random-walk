import RandomWalkModel exposing (..)
import RandomWalkView exposing (..)

import Signal exposing (..)
import Window exposing (..)
import Time exposing (..)
import Graphics.Element exposing (..)



timeSig : Signal Time
timeSig = Time.every (Time.millisecond * 10)


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
  map2 inp timeSig panelDimSig



modelSig : Signal Model
modelSig =
  let
    initialModel = initial 39398127
  in
    Signal.foldp update initialModel inpSig


main : Signal Element
main =
  Signal.map2 view panelDimSig modelSig
