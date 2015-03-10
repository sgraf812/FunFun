import Keyboard
import Window
import Debug
import Graphics.Element (..)
import Graphics.Collage (..)
import Color (..)
import Time (fps)
import Signal
import List (..)

-- GENERIC

between : number -> number -> number -> Bool
between min max x =
  x >= min && x < max

iterate : (a -> a) -> Int -> a -> a
iterate f n =
  foldr (<<) identity (repeat n f)

-- MODEL

type alias World = List Sprite

type Sprite = Sky | Platform PlatformSprite | Player BasicSprite

type alias BasicSprite =
    { x   : Float
    , y   : Float
    , w   : Float
    , h   : Float
    , vx  : Float
    , vy  : Float
    , dir : Direction
    }

type alias PlatformSprite =
    { x   : Float
    , y   : Float
    , w   : Float
    , h   : Float
    , c   : Color
    }

type Direction = Left | Right

type alias Keys = { x:Int, y:Int }

start_state : World
start_state = [
    Player { x = 0
    , y = 0 
    , w = 16
    , h = 26
    , vx = 0
    , vy = 0
    , dir = Right
    },
    Platform { x = 40
    , y = 20 
    , w = 20
    , h = 20
    , c = red
    }, 
    Platform { x = 60
    , y = 30 
    , w = 20
    , h = 30
    , c = red
    }, 
    -- the floor
    Platform { x = 0
    , y = 0 
    , w = 9999
    , h = 50
    , c = rgb 74 167 43
    }, 
    -- the sky
    Sky ]


-- UPDATE

step : (Float, Keys) -> World -> World
step move world =
  let step = stepOne move
  in
    map (stepOne move world) world

stepOne : (Float, Keys) -> World -> Sprite -> Sprite
stepOne dims world sprite = case sprite of
  Player sprite' -> Player (stepPlayer dims world sprite')
  _ -> sprite

stepPlayer : (Float, Keys) -> World -> BasicSprite -> BasicSprite
stepPlayer (dt, keys) world mario =
    let n = 6 in
    mario
        |> jump keys
        |> walk keys
        |> iterate (physics (dt/n) world) n

jump : Keys -> BasicSprite -> BasicSprite
jump keys mario =
    if keys.y > 0 && mario.vy == 0 then { mario | vy <- 4.0 } else mario

supports mario p =
  case p of
    Platform pl -> between (pl.y-pl.h) pl.y mario.y && between (pl.x-pl.w/2) (pl.x+pl.w/2) mario.x
    _ -> False

physics : Float -> World -> BasicSprite -> BasicSprite
physics dt world mario =
    let newy = mario.y + dt * mario.vy - 0.01 -- fudge factor to "test" our feet
        support = filter (supports {mario | y <- newy}) world
        newv = if (isEmpty support) then mario.vy - dt/8 else 0
        f = Debug.watch "floor" support
    in
    Debug.watch "mario" <| 
    { mario |
        x <- mario.x + dt * mario.vx,
        y <- if (isEmpty support) then newy else mario.y,
        vy <- newv
    }

walk : Keys -> BasicSprite -> BasicSprite
walk keys mario =
    { mario |
        vx <- toFloat keys.x,
        dir <- if | keys.x < 0 -> Left
                  | keys.x > 0 -> Right
                  | otherwise  -> mario.dir
    }


-- DISPLAY

display : (Int, Int) -> World -> Element
display (w',h') world =
  collage w' h' (concatMap (displayOne (w',h')) (reverse world))

displayOne : (Int, Int) -> Sprite -> List Form
displayOne dims sprite = case sprite of
  Player basic -> displayPlayer dims basic
  Platform basic -> displayPlatform dims basic
  Sky -> displaySky dims

displayPlayer : (Int, Int) -> BasicSprite -> List Form
displayPlayer (w',h') mario =
  let (w,h) = (toFloat w', toFloat h')

      verb = if | (abs mario.vy) > 0 -> "jump"
                | mario.vx /= 0 -> "walk"
                | otherwise     -> "stand"

      dir = case mario.dir of
              Left -> "left"
              Right -> "right"

      src  = "imgs/mario/"++ verb ++ "/" ++ dir ++ ".gif"

      marioImage = image 35 35 src

      base = 50
  in [marioImage
        |> toForm
        |> Debug.trace "mario"
        |> move (mario.x, mario.y + mario.h/2 + base - h/2) ]

displayPlatform : (Int, Int) -> PlatformSprite -> List Form
displayPlatform (w',h') pl =
  let (w,h) = (toFloat w', toFloat h')
      pw = min w pl.w
      ph = min h pl.h
      base = 50
  in
          [ rect pw ph
              |> filled pl.c
              |> move (pl.x, pl.y - ph/2 + base - h/2) ]

displaySky : (Int, Int) -> List Form
displaySky (w',h') =
  let (w,h) = (toFloat w', toFloat h')
  in
          [ rect w h |> filled (rgb 174 238 238) ]          

-- SIGNALS

main : Signal Element
main =
  let states = Signal.foldp step start_state input
  in
    Signal.map2 display Window.dimensions states

input : Signal (Float, Keys)
input =
  let delta = Signal.map (\t -> t/20) (fps 30)
      deltaArrows = Signal.map2 (,) delta Keyboard.arrows
  in
      Signal.sampleOn delta deltaArrows