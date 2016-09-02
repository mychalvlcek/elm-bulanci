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


type alias Player =
  { x : Float
  , y : Float
  , dir : Direction 
  }

type alias Model =
  { keys : Keys 
  , size : Size
  , players: Dict.Dict String Player
  }


type Direction 
  = Left
  | Right
  | Top
  | Bottom

type alias Keys = 
  { x:Int
  , y:Int 
  }

init : Model
init =
  { size = Size 0 0 
  , keys = Keys 0 0
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
    37 -> { keys | x = -1} -- left arrow 
    39 -> { keys | x = 1} -- right arrow
    38 -> { keys | y = 1} -- up arrow
    40 -> { keys | y = -1} -- bottom arrow
    (-37) -> { keys | x = 0}
    (-39) -> { keys | x = 0}
    (-38) -> { keys | y = 0}
    (-40) -> { keys | y = 0}
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
  | players = 
    (Dict.fromList 
      (List.map (\(key, player) -> 
        (key,
          player
            |> walk model.keys
            |> physics dt model.keys))
      (Dict.toList model.players)))
  }


physics : Float -> Keys -> Player -> Player
physics dt keys player =
  { player |
    x = player.x + toFloat keys.x * 2,
    y = player.y + toFloat keys.y * 2
  }

walk : Keys -> Player -> Player
walk keys player =
  { player |
    dir = 
      case (compare keys.x 0, compare keys.y 0) of 
        (LT,EQ) -> Left
        (GT,EQ) -> Right
        (EQ,LT) -> Bottom
        (EQ,GT) -> Top
        _ -> player.dir -- default direction
  }


-- VIEW 

renderImage keys player = 
  let
    verb =
      case (keys.x > 0 || keys.y > 0) of
        True -> ""
        False -> ""

    dir = case player.dir of
            Left -> "left"
            Right -> "right"
            Top -> "back"
            Bottom -> "front"

    src  = "images/" ++ verb ++ "/" ++ dir ++ ".png"
    bulanek = image 35 35 src
  in
    bulanek
      |> toForm
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
        , List.map (\player -> renderImage model.keys player) (Dict.values model.players)
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
