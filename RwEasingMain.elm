import RwEasingModel exposing (..)
import RwEasingView exposing (..)

import Graphics.Element exposing (..)
import Signal exposing (..)
import Time exposing (..)

timeSig : Signal Time
timeSig = foldp (\a b -> b + 10) 0 (Time.fps 10)

main : Signal Element
main = Signal.map showAnim timeSig
