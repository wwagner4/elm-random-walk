module RwPointMain where

import RwPointModel exposing (..)
import RwPointView exposing (..)

import Graphics.Element exposing (..)
import Signal exposing (..)
import Time exposing (..)
import Window exposing (..)
import Maybe exposing (..)

timeSig : Signal Time
timeSig = every (Time.millisecond * 10)


toPanelSize : (Int, Int) -> PanelSize
toPanelSize (w, h) = 
  { w = toFloat w
  , h = toFloat h }


panelSizeSig : Signal PanelSize
panelSizeSig = Signal.map toPanelSize Window.dimensions


toInp : Time -> PanelSize -> Inp
toInp time panelSize = 
  { time = time
  , panelSize = panelSize }


inpSig : Signal Inp
inpSig = Signal.map2 toInp timeSig panelSizeSig 


modelSig : Signal (Maybe Model)
modelSig = foldp updateModel Nothing inpSig


main : Signal Element
main = Signal.map2 viewModel Window.dimensions modelSig
