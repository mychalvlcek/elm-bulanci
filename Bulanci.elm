import Html exposing (..)
import Keyboard
import Window exposing (Size)
import AnimationFrame
import Dict

import Task 
import Html.App as App

import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)


-- MODEL
type Direction 
  = Left
  | Right
  | Top
  | Bottom

type alias Keys = 
  { x:Int
  , y:Int
  , p2x : Int
  , p2y : Int
  }

type alias Player =
  { x : Float
  , y : Float
  , dir : Direction 
  }

type alias Model =
  { keys : Keys 
  , size : Size
  , movementAngle : Int
  , players: Dict.Dict String Player
  }

init : Model
init =
  { size = Size 0 0 
  , keys = Keys 0 0 0 0
  , movementAngle = 0
  , players = Dict.fromList
    [ ("a", Player 0 0 Top)
    , ("b", Player 100 -100 Left)
    ]
  }


-- UPDATE

type Msg 
  = DoNothing
  | Tick Float 
  | SizeChange Size
  | KeyDown Int
  | KeyUp Int

updateKeys : Int -> Keys -> Keys
updateKeys key keys =
  case key of
    -- arrow keys
    37 -> { keys | x = -1}
    39 -> { keys | x = 1}
    38 -> { keys | y = 1}
    40 -> { keys | y = -1}
    (-37) -> { keys | x = 0}
    (-39) -> { keys | x = 0}
    (-38) -> { keys | y = 0}
    (-40) -> { keys | y = 0}
    -- WSAD
    65 -> { keys | p2x = -1}
    68 -> { keys | p2x = 1}
    87 -> { keys | p2y = 1}
    83 -> { keys | p2y = -1}
    (-65) -> { keys | p2x = 0}
    (-68) -> { keys | p2x = 0}
    (-83) -> { keys | p2y = 0}
    (-87) -> { keys | p2y = 0}
    _ -> keys 

update : Msg -> Model -> Model
update msg model =
  case msg of 
    KeyDown key ->
      { model | keys = updateKeys key model.keys}
    KeyUp key ->
      { model | keys = updateKeys -key model.keys }
    SizeChange size ->
      { model | size = size}  
    Tick dt -> 
      step dt model 
    DoNothing -> model

step : Float -> Model -> Model
step dt model =
  { model
  | movementAngle =
      case model.movementAngle > 12 of
        True -> -12
        False -> model.movementAngle + 2
  , players = 
    (Dict.fromList 
      (List.map (\(key, player) -> 
        (key,
          player
            |> walk model.keys key
            |> physics dt model.keys key))
      (Dict.toList model.players)))
  }


physics : Float -> Keys -> String -> Player -> Player
physics dt keys playerId player =
  let playerKeys =
    case playerId == "a" of
      True -> (keys.x, keys.y)
      False -> (keys.p2x, keys.p2y)
  in
  { player |
    x = player.x + toFloat (fst playerKeys) * dt / 6,
    y = player.y + toFloat (snd playerKeys) * dt / 6
  }

walk : Keys -> String -> Player -> Player
walk keys playerId player =
  let playerKeys =
    case playerId == "a" of
      True -> (compare keys.x 0, compare keys.y 0)
      False -> (compare keys.p2x 0, compare keys.p2y 0)
  in
  { player |
    dir = case playerKeys of
        (LT,EQ) -> Left
        (GT,EQ) -> Right
        (EQ,LT) -> Bottom
        (EQ,GT) -> Top
        _ -> player.dir -- default direction
  }


-- VIEW 

renderImage keys playerId movementAngle player =
  let
    playerKeys =
      case playerId == "a" of
        True -> (keys.x /= 0 || keys.y /= 0)
        False -> (keys.p2x /= 0 || keys.p2y /= 0)
    movementType =
      case playerKeys of
        True -> "gun"
        False -> "reload"
    directionRotation =
      case player.dir of
        Left -> 180
        Right -> 0
        Top -> 90
        Bottom -> -90
    extraAngle =
      case playerKeys of
        True -> toFloat movementAngle / 3
        False -> 0

    bulanek = image 35 43 ("images/characters/man-old/manOld_" ++ movementType ++ ".png")
  in
    bulanek
      |> toForm
      |> rotate (degrees (directionRotation + extraAngle))
      |> move (player.x, player.y)

view : Model -> Html msg
view model =
  let 
    (w', h') = (model.size.width, model.size.height)
    (w, h) = (toFloat w', toFloat h')
   
  in
    collage w' h'
        (List.concat
        [
          [ rect w h |> filled (rgb 74 167 43) ]
        , List.map (\(key, player) -> renderImage model.keys key model.movementAngle player) (Dict.toList model.players)
        ])
    |> toHtml

-- WIRING


main : Program Never
main =
  App.program
    { init = (init, Task.perform (\_ -> DoNothing) SizeChange Window.size)
    , update = \msg model -> (update msg model, Cmd.none)
    , view = view
    , subscriptions = 
        \_ -> Sub.batch 
          [ Window.resizes SizeChange
          , Keyboard.downs KeyDown
          , Keyboard.ups KeyUp
          , AnimationFrame.diffs Tick] 
    }
