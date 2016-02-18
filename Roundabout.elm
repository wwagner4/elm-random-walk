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
  { spec : StateSpec
  , start : Time
  , duration : Time }


newState : StateSpec -> Time -> Time -> Model
newState spec start duration =
  { spec = spec
  , start = start
  , duration = duration }


type StateSpec = A | B | C

type Sig = SigReady | SigProcessing


updateState : Time -> Maybe Model -> Maybe Model
updateState time maybeState =
  let
    sig : Model -> Sig
    sig state =
      if ((time - state.start) > state.duration) then SigReady
      else SigProcessing


    state = withDefault (initial time) maybeState
    nextState = case sig state of
      SigReady ->
        case state.spec of
          A -> newState B time (Time.second * 2)
          B -> newState C time (Time.second * 0.5)
          C -> newState A time (Time.second * 0.2)
      SigProcessing -> state
  in
    Just nextState


initial : Time -> Model
initial time = newState A time Time.second


view : (Int, Int) -> Maybe Model -> Element
view (w, h) maybeState =
  let
    viewModel : Model -> List Form
    viewModel state =
      let
        height = 200
        (txt, color) = case state.spec of
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
      Just state -> viewModel state
  in
    collage w h elems


modelSig : Signal (Maybe Model)
modelSig = Signal.foldp updateState Nothing (every (Time.second / 10))


main = Signal.map2 view dimensions modelSig
