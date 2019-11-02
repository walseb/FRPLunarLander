{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main
  ( main,
  )
where

import Control.Concurrent
  ( newMVar,
    swapMVar, --, threadDelay )
  )
import Control.Lens
import Data.String (fromString)
import FRP.BearRiver as B
import FRP.Yampa as Y
import Foreign.C.Types
  ( CDouble,
    CInt,
  )
import Input
import Linear
import qualified SDL as S
import qualified Sprite as SP
import Types ()

newtype Size = Size (V2 CInt)

data Object
  = Object
      { _pos :: V2 CInt,
        _size :: Size,
        _rot :: CDouble,
        _alive :: Bool
      }

makeLenses ''Object

data Resources = Resources {_objectSprite :: SP.Sprite}

makeLenses ''Resources

data Objects
  = Objects
      { _player :: Object,
        _enemies :: [Object]
      }

makeLenses ''Objects

data GameState
  = GameState
      { _objects :: Objects
      }

makeLenses ''GameState

spritePath :: FilePath
spritePath = "data/testSprite.png"

initialGame =
  GameState
    ( Objects
        (Object (V2 0 0) (Size (V2 500 500)) 0 True)
        [(Object (V2 200 1000) (Size (V2 500 500)) 0 True)]
    )

collides :: (V2 CInt, Size) -> (V2 CInt, Size) -> Bool
collides
  -- Box1
  (V2 x0 y0, Size (V2 sizeX0 sizeY0))
  -- Box2
  (V2 x1 y1, Size (V2 sizeX1 sizeY1)) =
    x0 < x1 + sizeX1
      && x0 + sizeX0 > x1
      && y0 < y1 + sizeY1
      && sizeY0 + y0 > y1

checkCollisionsEvent :: Y.SF ((V2 CInt, Size), (V2 CInt, Size)) (Bool, Y.Event ())
checkCollisionsEvent = proc state ->
  returnA -< case uncurry collides state of
    True -> (False, Y.Event ())
    False -> (True, Y.NoEvent)

checkCollisions :: Y.SF ((V2 CInt, Size), (V2 CInt, Size)) Bool
checkCollisions =
  B.switch checkCollisionsEvent (\_ -> constant False)

objectSpeed :: V2 Double
objectSpeed = 100

objectGravity = V2 0 100

shipMovement :: V2 Double -> V2 Double -> Y.SF Bool (V2 CInt, Y.Event (V2 Double, V2 Double))
shipMovement initialPos initialVelocity = proc input -> do
  v <- Y.integralFrom initialVelocity -< objectGravity
  p <- Y.integralFrom initialPos -< v
  returnA -<
    ( fmap floor p,
      case input of
        True -> Y.Event (p, v)
        False -> Y.NoEvent
    )

shipThrusters :: V2 Double -> V2 Double -> Y.SF Bool (V2 CInt)
shipThrusters initialPos initialVel =
  B.switch
    (Y.iPre False >>> shipMovement initialPos initialVel)
    (\(pos, V2 velX velY) -> shipThrusters pos (V2 velX (velY - 10)))

shipControl :: V2 Double -> V2 Double -> Y.SF MovementVector (CDouble, V2 CInt)
shipControl initialPos initialVel = proc movement -> do
  let movement2 = movement ^. (up . pressed)
  pos <- shipThrusters initialPos initialVel -< movement2
  shipRotation' 0 -< (fmap fromIntegral pos, movement ^. (Input.left . pressed), movement ^. (Input.right . pressed))

-- shipThrusters :: V2 Double -> V2 Double -> Y.SF (Bool Bool Bool) (V2 CInt)
-- shipThrusters initialPos initialVel =
--   B.switch
--     (Y.iPre False >>> shipMovement initialPos initialVel)
--     (\(pos, V2 velX velY) -> shipThrusters pos (V2 velX (velY - 10)))

-- Rotates either left or right
shipRotation' :: CDouble -> Y.SF (V2 Double, Bool, Bool) (CDouble, V2 CInt)
shipRotation' initialRot = proc (V2 posX posY, left, right) -> do
  angle <- shipRotation initialRot -< (left, right)
  let originX = posX / 2
  let originY = posY / 2
  let angleGood = pi / 180 :: Double
  let test =
        ( V2
            (cos angleGood * (posX - originX) - sin angleGood * (posY - originY) + originX)
            (sin angleGood * (posX - originX) + cos angleGood * (posY - originY) + originY)
        )
  returnA -< (angle, fmap ceiling test)

--   The calculation needed here is a "rotate point around origin" right
--   BUT the origin is not the bottom right point
--         Point p1->  +-------+
--                    |       |
--                    |   .   |  . = Origin!!! So origin is (p1 / 2)
--                    |       |
--                    +-------+ <- NOT Origin, not anything
-- Then p1 just rotates around origin!!!! In a circle!!

-- https://stackoverflow.com/questions/4465931/rotate-rectangle-around-a-point

shipRotation :: CDouble -> Y.SF (Bool, Bool) CDouble
shipRotation initialRot = proc (left, right) -> do
  -- Stop moving or something if a collision is detected
  p <- B.integralFrom initialRot -< ((boolToInt left - boolToInt right) * 50)
  returnA -< p
  where
    boolToInt :: Bool -> CDouble
    boolToInt a = if a then 0 else 1

movingObject :: V2 Double -> Y.SF (V2 CInt) (V2 CInt)
movingObject initialPos = proc move -> do
  -- Stop moving or something if a collision is detected
  p <- Y.integralFrom initialPos -< objectSpeed * fmap fromIntegral move
  returnA -< fmap floor p

applyInputs :: GameState -> Y.SF InputState Objects
applyInputs initialGameState = proc input -> do
  -- Calculate new pos without modifying state
  (rot, playerPos) <- shipControl 0 0 -< (input ^. movement)
  -- playerRot <- shipRotation 0 -< (input ^. (Input.left . pressed), input ^. (Input.right . pressed))
  -- playerPos, playerRot <- shipRotation' 0 -< (input ^. (Input.left . pressed), input ^. (Input.right . pressed))

  -- playerPos <- movingObject (fmap fromIntegral (initialGameState ^. (objects . player . pos))) -< vectorizeMovement input
  let playerSize = initialGameState ^. (objects . player . size)
  enemyPos <- movingObject (fmap fromIntegral (initialGameState ^. (objects . enemies . to head . pos))) -< (V2 0 (-1))
  let enemySize = initialGameState ^. (objects . enemies . to head . size)
  isPlayerAlive <- checkCollisions -< ((playerPos, (Size (V2 500 500))), (enemyPos, enemySize))
  let objectState =
        ( Objects
            -- Player
            (Object playerPos playerSize rot isPlayerAlive)
            -- Test
            [(Object enemyPos enemySize 45 True)]
        )
  returnA -< objectState

update :: GameState -> Y.SF (Y.Event [S.Event]) (GameState, Bool)
update origGameState = proc events -> do
  newInputState <- accumHoldBy inputStateUpdate keybinds -< events
  gameState <- applyInputs origGameState -< newInputState
  returnA -<
    ( GameState gameState,
      newInputState ^. (Input.quit . pressed)
    )

render :: S.Renderer -> Resources -> (GameState, Bool) -> IO Bool
render renderer (Resources sprite) (GameState (Objects player objects), exit) =
  do
    -- threadDelay 100000

    S.rendererDrawColor renderer S.$= S.V4 0 0 100 255
    S.clear renderer
    case (player ^. alive) of
      True ->
        SP.renderEx
          sprite
          (player ^. pos)
          Nothing
          (V2 500 500)
          (player ^. rot)
          Nothing
          (V2 False False)
      False ->
        pure ()
    SP.render sprite (objects ^. (to head . pos)) Nothing (V2 500 500)
    S.present renderer
    return exit

main :: IO ()
main = do
  S.initializeAll
  window <- S.createWindow (fromString "My SDL Application") S.defaultWindow
  renderer <- S.createRenderer window (-1) S.defaultRenderer
  spritetest <- SP.load renderer spritePath (V2 500 500)
  let resources = Resources spritetest
  lastInteraction <- newMVar =<< S.time
  let senseInput _canBlock = do
        currentTime <- S.time
        dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
        events <- Y.Event <$> S.pollEvents
        return (dt, Just events)
  reactimate (return Y.NoEvent) senseInput (\_ -> render renderer resources) (update initialGame)
  S.destroyRenderer renderer
  S.destroyWindow window
