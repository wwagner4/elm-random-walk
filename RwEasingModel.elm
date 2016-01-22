import Time exposing (..)
import Easing exposing (..)
import Graphics.Element exposing (..)


showTime : Time -> Element
showTime time = show time


timeSig : Signal Time
timeSig = Time.every (second)


main : Signal Element
main = Signal.map showTime timeSig
