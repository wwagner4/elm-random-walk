import RwEasingModel exposing (..)
import RwEasingView exposing (..)

import Graphics.Element exposing (..)
import Signal exposing (..)
import Time exposing (..)
import Window exposing (..)


timeSig : Signal Time
timeSig = foldp (\a b -> b + 10) 0 (Time.fps 10)

modelSig : Signal Model
modelSig = foldp updateModel initial timeSig

main : Signal Element
main = Signal.map2 viewModel Window.dimensions modelSig
