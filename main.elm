import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Window
import Debug exposing (..)


-- MODEL --

fieldSize = { w = 400, h = 400 }
fieldBounds =
    {
        left = -fieldSize.w / 2,
        right = fieldSize.w / 2,
        top = fieldSize.h / 2,
        bottom = -fieldSize.h / 2
    }

type alias Model = {
    balls : List Ball
}

type alias Ball = {
    id: Int,
    radius : Float,
    x : Float,
    y: Float,
    vx: Float,
    vy: Float,
    ax: Float,
    ay: Float,
    color: Color,
    mass: Float
}

left ball = ball.x - ball.radius
right ball = ball.x + ball.radius
top ball = ball.y + ball.radius
bottom ball = ball.y - ball.radius

cr : Float
cr = 0.95

ball1 = { id=0, radius=10, x=0, y=0, vx=0.1, vy=0.1, ax=0, ay=0, color=green, mass=1 }
ball2 = { id=1, radius=10, x=40, y=10, vx=-0.1, vy=0, ax=0, ay=0, color=red, mass=1 }
ball3 = { id=2, radius=10, x=-30, y=65, vx=0.3, vy=0.1, ax=0, ay=0, color=yellow, mass=1 }
ball4 = { id=3, radius=10, x=35, y=-30, vx=-0.1, vy=-0.1, ax=0, ay=0, color=blue, mass=1 }
ball11 = { id=10, radius=10, x=-100, y=0, vx=0.1, vy=0.1, ax=0, ay=0, color=green, mass=1 }
ball12 = { id=11, radius=10, x=100, y=10, vx=-0.1, vy=0, ax=0, ay=0, color=red, mass=1 }
ball13 = { id=12, radius=10, x=-30, y=-165, vx=0.3, vy=0.1, ax=0, ay=0, color=yellow, mass=1 }
ball14 = { id=13, radius=10, x=35, y=-130, vx=-0.1, vy=-0.1, ax=0, ay=0, color=blue, mass=1 }
ball21 = { id=20, radius=10, x=0, y=-100, vx=0.1, vy=0.1, ax=0, ay=0, color=green, mass=1 }
ball22 = { id=21, radius=10, x=40, y=110, vx=-0.1, vy=0, ax=0, ay=0, color=red, mass=1 }
ball23 = { id=22, radius=10, x=-30, y=165, vx=0.3, vy=0.1, ax=0, ay=0, color=yellow, mass=1 }
ball24 = { id=23, radius=10, x=135, y=-30, vx=-0.1, vy=-0.1, ax=0, ay=0, color=blue, mass=1 }
ball211 = { id=210, radius=10, x=-100, y=50, vx=0.1, vy=0.1, ax=0, ay=0, color=green, mass=1 }
ball212 = { id=211, radius=10, x=100, y=110, vx=-0.1, vy=0, ax=0, ay=0, color=red, mass=1 }
ball213 = { id=212, radius=10, x=-130, y=-165, vx=0.3, vy=0.1, ax=0, ay=0, color=yellow, mass=1 }
ball214 = { id=213, radius=10, x=35, y=-200, vx=-0.1, vy=-0.1, ax=0, ay=0, color=blue, mass=1 }

initialState : Model
initialState =
    {
        balls = [ ball1, ball2, ball3, ball4, ball11, ball12, ball13, ball14, ball21, ball22, ball23, ball24, ball211, ball212, ball213, ball214 ]
    }

-- UPDATE --

update : Time -> Model -> Model
update dt state =
    physics dt state

physics : Time -> Model -> Model
physics dt state =
    { state |
        balls = state.balls
            |> collideBallsWithWalls
            |> collideBallsWithBalls
            |> List.map (physicsBall dt)
    }

physicsBall : Time -> Ball -> Ball
physicsBall dt ball =
    let
        newVx = ball.vx + dt * ball.ax
        newVy = ball.vy + dt * ball.ax
    in
        { ball |
            x = ball.x + dt * newVx,
            y = ball.y + dt * newVy,
            vx = newVx,
            vy = newVy
        }

type alias Collision = (Ball, Ball)

collideBallsWithWalls : List Ball -> List Ball
collideBallsWithWalls balls =
    balls |>
        List.map (\ball ->
            if left ball < fieldBounds.left then { ball | x = fieldBounds.left + ball.radius, vx = -ball.vx, ax = -ball.ax }
            else if top ball > fieldBounds.top then { ball | y = fieldBounds.top - ball.radius, vy = -ball.vy, ay = -ball.ay }
            else if right ball > fieldBounds.right then { ball | x = fieldBounds.right - ball.radius, vx = -ball.vx, ax = -ball.ax }
            else if bottom ball < fieldBounds.bottom then { ball | y = fieldBounds.bottom + ball.radius, vy = -ball.vy, ax = -ball.ax }
            else ball
            )

collideBallsWithBalls : List Ball -> List Ball
collideBallsWithBalls balls =
    balls |>
        List.map (\ball ->
            case collisionForBall ball balls of
                Just col -> ballAfterCollision col
                Nothing -> ball
        )

collisionForBall : Ball -> List Ball -> Maybe Collision
collisionForBall ball balls =
    List.foldl (\currentBall maybeCollision ->
        case maybeCollision of
            Just collision -> log "Collision!" (Just collision)
            Nothing -> collisionBetweenBalls ball currentBall
        ) Nothing balls

collisionBetweenBalls: Ball -> Ball -> Maybe Collision
collisionBetweenBalls ball otherBall =
    let
        distanceSquared = (otherBall.x - ball.x)^2 + (otherBall.y - ball.y)^2
        radiSquared = (otherBall.radius + ball.radius) ^2
    in
        if ball.id == otherBall.id then Nothing
        else if distanceSquared < radiSquared then Just (ball, otherBall)
        else Nothing

ballAfterCollision : Collision -> Ball
ballAfterCollision col =
    let
        ballA = fst col
        ballB = snd col
        distance = sqrt ( (ballB.x - ballA.x)^2 + (ballB.y - ballA.y)^2 )
        radi = ballA.radius + ballB.radius
        penetration = radi - distance
        unitVectorFromAtoB = { x = (ballB.x - ballA.x) / distance, y = (ballB.y - ballA.y) / distance }
        penetrationVector = { x = penetration * (unitVectorFromAtoB.x) , y = penetration * (unitVectorFromAtoB.y) }
        minimumTranslationVector = { x = -(penetrationVector.x / 2),  y = -(penetrationVector.y / 2) }
    in
        { ballA |
            x = ballA.x + minimumTranslationVector.x,
            y = ballA.y + minimumTranslationVector.y,
            vx = velocityAfterCollision (ballA.vx, ballA.mass) (ballB.vx, ballB.mass),
            vy = velocityAfterCollision (ballA.vy, ballA.mass) (ballB.vy, ballB.mass)
        }

velocityAfterCollision : (Float, Float) -> (Float, Float) -> Float
velocityAfterCollision (va, ma) (vb, mb) = (cr * mb * (vb - va) + ma * va + mb * vb) / (ma + mb)

-- VIEW --

view : (Float, Float) -> Model -> Element
view (w, h) state =
    collage (round w) (round h) [
        viewBalls state.balls
    ]

viewBalls : List Ball -> Form
viewBalls balls =
    group (List.map viewBall balls)

viewBall : Ball -> Form
viewBall ball =
    circle ball.radius
        |> filled ball.color
        |> move (ball.x, ball.y)

-- SIGNALS --

tick : Signal Time
tick = fps 60

updatedModel : Signal Model
updatedModel = Signal.foldp update initialState tick

main : Signal Element
main = Signal.map (view (fieldSize.w, fieldSize.h))  updatedModel
