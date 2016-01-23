import Time exposing (..)
import Easing exposing (..)
import Signal exposing (..)
import Graphics.Element exposing (..)
import Maybe exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)



type alias Model =
  { x : Float
  , y : Float
  , startTime : Time }



timeSig : Signal Time
timeSig = foldp (\a b -> b + 10) 0 (Time.fps 10)


anim : Time -> Float
anim currentTime =
    ease easeOutBack float 0 500 second currentTime


showModel : Model -> Element
showModel model =
  let
    shape = circle 20
    form = shape
      |> filled Color.red
      |> alpha 0.4
      |> move (model.x, model.y)
  in
    collage 500 500 [form]



showTime : Time -> Element
showTime time = show time


showAnim : Time -> Element
showAnim time =
  show (time, (anim time))


main : Signal Element
main = Signal.map showAnim timeSig
