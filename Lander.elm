import Signal
import Signal.Extra
import Graphics.Element (..)
import Graphics.Collage (..)
import Transform2D (..)
import Color (..)
import Time (..)
import List (..)
import Debug
import Window
import Keyboard
-- App imports
import Generic (..)

-- Model

-- better use elm-linear-algebra at a later stage
type alias Vector = { x : Float, y : Float }

type alias Shippy = { heading : Float }

type alias Planety = { radius : Float }


mapVector : (Float -> Float) -> Vector -> Vector
mapVector f { x, y } =
    { x = f x
    , y = f y
    }

mapVector2 : (Float -> Float -> Float) -> Vector -> Vector -> Vector
mapVector2 f a b =
    { x = f a.x b.x
    , y = f a.y b.y
    }

xy : Float -> Vector
xy s =
    { x = s
    , y = s
    }

lenVector : Vector -> Float
lenVector { x, y } =
    sqrt (x^2 + y^2)

addVector = mapVector2 (+)
subVector = mapVector2 (-)
mulVector = mapVector2 (*)
divVector = mapVector2 (/)

type BodyKind 
    = Ship { heading : Float }
    | Planet { radius : Float }

type alias Body = 
    { position : Vector
    , velocity : Vector
    , mass : Float
    , kind : BodyKind
    }

start viewport = case viewport of
    Viewport (w,h) -> {
        view = {w = w, h = h},
        bodies = 
            [ { kind = Ship { heading = 0 }
              , position = { x = 0, y = 0 }
              , velocity = { x = 0, y = 0.07 }
              , mass = 0.001
              }

            , { kind = Planet { radius = 70 }
              , position = { x = -150, y = 0 }
              , velocity = { x = 0, y = 0 }
              , mass = 1
              }

            ]
    }

-- Update

type Update = Viewport (Int, Int) | Tick Float | Move {x:Int,y:Int}

update s world = case s of
    Viewport dims -> updateViewport dims world
    Tick dt -> let n = 8 in iterate (updateTick (dt/n)) n world
    Move arrows -> updateMove arrows world

updateViewport (w,h) world =
    let wv = world.view
        v' = { wv | w <- 0, h <- 0}
    in {world | view <- v'}

attraction : Body -> Body -> Vector 
attraction to from = -- the attraction vector points from `to` to `from`, because `from` is where the attraction comes from
    let diff = from.position `subVector` to.position
        distance = lenVector diff
        dir = diff `divVector` xy distance -- normalization step
    in xy (from.mass/distance^2) `mulVector` dir


updateTick dt world =
    let accumulatedAcceleration body = 
            world.bodies 
            |> filter ((/=) body) 
            |> map (attraction body) -- attractive forces on `body`
            |> foldl addVector (xy 0) -- basically `sum` on `Vector`
        updateMovement body =
            case body.kind of -- we only want to update ships
                Ship _ ->
                    let velocity = 
                            accumulatedAcceleration body
                            |> mulVector (xy dt) -- integrate accelerating force over time => velocity
                            |> addVector body.velocity
                        position =
                            velocity
                            |> mulVector (xy dt) -- integrate velocity over time => position
                            |> addVector body.position
                    in { body | velocity <- velocity, position <- position }
                _ -> body
    in { world | bodies <- map updateMovement world.bodies }

isShip : Body -> Bool
isShip body =
    case body.kind of
        Ship _ -> True
        _ -> False

updateMove arrows world = 
    let updateBody body =
            case body.kind of
                Ship {heading} -> 
                    { body 
                    | kind <- Ship { heading = heading - toFloat arrows.x * 5 }
                    , velocity <-
                        { x = body.velocity.x - (toFloat arrows.y) * sin (degrees heading) * 0.001
                        , y = body.velocity.y + (toFloat arrows.y) * cos (degrees heading) * 0.001
                        }
                    }
                _ -> body
    in { world | bodies <- map updateBody world.bodies }

-- Display

displayBody body =
    case body.kind of
        Ship {heading} -> Debug.trace "ship" <| move (body.position.x, body.position.y)
            <| rotate (degrees heading)
            <| group [  rotate (degrees -30) (outlined (solid white) (ngon 3 25)),
                        outlined (solid white) (rect 3 25)]
        Planet {radius} -> move (body.position.x, body.position.y) <| outlined (solid white) (circle radius)

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
    in collage world.view.w world.view.h <|
            append [filled black (rect w' h')] (map displayBody world.bodies)

-- Signals

window = Signal.map Viewport Window.dimensions
ticks = Signal.map Tick (fps 30)
keys = Signal.sampleOn (fps 30) (Signal.map Move Keyboard.arrows)

inputs = Signal.mergeMany [window, ticks, keys]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states