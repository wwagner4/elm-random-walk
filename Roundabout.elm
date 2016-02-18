module Roundabout where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Text exposing (..)
import Time exposing (..)
import Maybe exposing (..)
import Signal exposing (..)
import Window exposing (..)
import Color exposing (..)


type alias Model =
  { state : State
  , start : Time
  , duration : Time }


newState : State -> Time -> Time -> Model
newState state start duration =
  { state = state
  , start = start
  , duration = duration }


type State = A | B | C

type StateOfState = StateOfStateReady | StateOfStateProcessing


updateState : Time -> Maybe Model -> Maybe Model
updateState time maybeState =
  let
    sig : Model -> StateOfState
    sig state =
      if ((time - state.start) > state.duration) then StateOfStateReady
      else StateOfStateProcessing


    state = withDefault (initial time) maybeState
    nextState = case sig state of
      StateOfStateReady ->
        case state.state of
          A -> newState B time (Time.second * 2)
          B -> newState C time (Time.second * 0.5)
          C -> newState A time (Time.second * 0.2)
      StateOfStateProcessing -> state
  in
    Just nextState


initial : Time -> Model
initial time = newState A time Time.second


view : (Int, Int) -> Maybe Model -> Element
view (w, h) maybeState =
  let
    viewModel : Model -> List Form
    viewModel model =
      let
        height = 200
        (txt, color) = case model.state of
          A -> (fromString "A", Color.lightOrange )
          B -> (fromString "B", Color.lightGreen )
          C -> (fromString "C", Color.lightYellow )
        bgForm = square height
          |> filled color
        txtForm = txt
          |> Text.height height
          |> monospace
          |> centered
          |> toForm
          |> move (0, height / 30)
      in
        [bgForm, txtForm]



    elems = case maybeState of
      Nothing -> []
      Just model -> viewModel model
  in
    collage w h elems


modelSig : Signal (Maybe Model)
modelSig = Signal.foldp updateState Nothing (every (Time.second / 10))


main = Signal.map2 view dimensions modelSig
