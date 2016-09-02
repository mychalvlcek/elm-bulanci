import Html exposing (..)
import Keyboard
import Window exposing (Size)
import AnimationFrame

import Task 
import Html.App as App

import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)


-- MODEL

type alias Model =
  { x : Float
  , y : Float
  , dir : Direction
  , keys : Keys 
  , size : Size
  }


type Direction 
  = Left
  | Right
  | Top
  | Bottom

type alias Keys = { x:Int, y:Int }

init : Model
init =
  { x = 0
  , y = 0 
  , dir = Right
  , keys = Keys 0 0 
  , size = Size 0 0 
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
  model
    |> walk 
    |> physics dt

physics : Float -> Model -> Model
physics dt model =
  { model |
    x = model.x + toFloat model.keys.x * 2,
    y = model.y + toFloat model.keys.y * 2
  }

walk : Model -> Model
walk model =
  { model |
    dir = 
      case (compare model.keys.x 0, compare model.keys.y 0) of 
        (LT,EQ) -> Left
        (GT,EQ) -> Right
        (EQ,LT) -> Bottom
        (EQ,GT) -> Top
        _ -> model.dir -- default direction
  }


-- VIEW 

view : Model -> Html msg
view model =
  let 
    (w', h') = (model.size.width, model.size.height)
    (w, h) = (toFloat w', toFloat h')
    verb = 
      case (model.keys.x > 0 || model.keys.y > 0) of
        True -> "" -- walk
        False -> "" -- stand 

    dir = case model.dir of
            Left -> "left"
            Right -> "right"
            Top -> "back"
            Bottom -> "front"

    src  = "images/"++ verb ++ "/" ++ dir ++ ".png"

    marioImage = image 35 35 src
  in
    collage w' h'
        [ rect w h
            |> filled (rgb 74 167 43)
        , marioImage
            |> toForm
            |> move (model.x, model.y)
        ]
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
