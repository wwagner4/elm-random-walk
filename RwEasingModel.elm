import Time exposing (..)
import Easing exposing (..)
import Signal exposing (..)
import Graphics.Element exposing (..)


showTime : Time -> Element
showTime time = show time


timeSig : Signal Time
timeSig = foldp (\a b -> b + 1) 0 (Time.fps 2)


anim : Time -> Float
anim currentTime =
    ease easeInCubic float 0 10 second currentTime


showAnim : Time -> Element
showAnim time =
  show (anim time)


main : Signal Element
main = Signal.map showTime timeSig
