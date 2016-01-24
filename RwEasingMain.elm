import RwEasingModel exposing (..)
import RwEasingView exposing (..)

import Graphics.Element exposing (..)
import Signal exposing (..)
import Time exposing (..)
import Window exposing (..)
import Maybe exposing (..)

timeSig : Signal Time
timeSig = every (Time.millisecond * 10)

modelSig : Signal (Maybe Model)
modelSig = foldp updateModel Nothing timeSig

main : Signal Element
main = Signal.map2 viewModel Window.dimensions modelSig
