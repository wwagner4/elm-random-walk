module Behaviour where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Text exposing (..)
import Time exposing (..)
import Maybe exposing (..)
import Signal exposing (..)
import Window exposing (..)
import Color exposing (..)
import List exposing (..)
import Random exposing (..)
import Easing exposing (..)


type alias Model =
  { state : State
  , pos : Pos
  , rot : Float
  , startTime : Time
  , duration : Time }


model : State -> Pos -> Float -> Time -> Time -> Model
model state pos rot startTime duration =
  { state = state
  , pos = pos
  , rot = rot
  , startTime = startTime
  , duration = duration }


modelA pos rot time = model (A (moveBehaviour pos time)) pos rot time (Time.second * 4)
modelB pos rot time = model B pos rot time (Time.second * 1)
modelC pos rot time = model C pos rot time (Time.second * 1)


type alias Pos =
  { x : Float
  , y : Float }


pos : Float -> Float -> Pos
pos x y = { x = x, y = y }

posZero : Pos
posZero = {x = 0, y = 0 }


updatePos : Pos -> Float -> Float -> Pos
updatePos pos dx dy =
  { x = pos.x + dx, y = pos.y + dy }


type alias MoveBehaviour =
  { startPos : Pos
  , endPos : Pos }

moveBehaviour : Pos -> Time -> MoveBehaviour
moveBehaviour pos time =
  let
    maxVal = 350

    gen : Float -> Generator Float
    gen value =
      if value < 0 then Random.float (-maxVal * 0.3) maxVal
      else Random.float -maxVal (maxVal * 0.3)

    s0 = initialSeed (round time)
    (dx, s1) = generate (gen pos.x) s0
    (dy, s2) = generate (gen pos.y) s1
  in
    { startPos = pos
    , endPos = { x = pos.x + dx, y = pos.y + dy } }


type State = A MoveBehaviour | B | C


type Transition = TransitionReady | TransitionProcessing


updateModel : Time -> Maybe Model -> Maybe Model
updateModel time maybeModel =
  let
    transitionOf : Model -> Transition
    transitionOf model =
      if ((time - model.startTime) > model.duration) then TransitionReady
      else TransitionProcessing


    updatePosOn : MoveBehaviour -> Model -> Pos
    updatePosOn moveBehaviour model =
      let
        relTime = time - model.startTime
        x = ease easeOutBounce Easing.float moveBehaviour.startPos.x moveBehaviour.endPos.x model.duration relTime
        y = ease easeOutBounce Easing.float moveBehaviour.startPos.y moveBehaviour.endPos.y model.duration relTime
      in
        { x = x, y = y }


    model = withDefault (initial time) maybeModel
    nextModel =
      case transitionOf model of
        TransitionReady ->
          case model.state of
            A moveBehaviour -> modelB model.pos model.rot time
            B -> modelC model.pos model.rot time
            C -> modelA model.pos model.rot time
        TransitionProcessing ->
          case model.state of
            A moveBehaviour -> { model | pos = (updatePosOn moveBehaviour model) }
            B -> model
            C -> { model | rot = model.rot + 0.1 }

  in
    Just nextModel


initial : Time -> Model
initial time = modelA (pos 0 0) 0 time


view1 : (Int, Int) -> Maybe Model -> Element
view1 (w, h) maybeModel = show maybeModel


view : (Int, Int) -> Maybe Model -> Element
view (w, h) maybeModel =
  let
    viewModel : Model -> List Form
    viewModel model =
      let
        size = 100

        bgForm : Color -> Form
        bgForm color = square (size * 1.4)
          |> filled color

        txtForm : String -> Form
        txtForm txt = txt
          |> fromString
          |> Text.height size
          |> centered
          |> toForm
          |> move(0, -size * 0.1)

        grp = case model.state of
          A _ -> group [bgForm Color.red, txtForm "A"]
          B -> group [bgForm Color.green, txtForm "B"]
          C -> group [bgForm Color.yellow, txtForm "C"]
        grpTransformed = grp
          |> move (model.pos.x, model.pos.y)
          |> rotate model.rot
      in
        [grpTransformed]


    forms = case maybeModel of
      Nothing -> []
      Just model -> viewModel model
  in
    collage w h forms


modelSignal : Signal (Maybe Model)
modelSignal = Signal.foldp updateModel Nothing (every (Time.millisecond * 100))


main = Signal.map2 view dimensions modelSignal
