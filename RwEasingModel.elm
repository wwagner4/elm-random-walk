import Time exposing (..)
import Easing exposing (..)
import Graphics.Element exposing (..)


showTime : Time -> Element
showTime time = show time


main : Signal Element
main = Signal.map showTime (Time.every (second / 100))
