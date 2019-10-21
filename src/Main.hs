{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Main ( main ) where

import FRP.BearRiver as B
import FRP.Yampa as Y

import SDL
import qualified SDL.Sprite

import Data.String ( fromString )

import Foreign.C.Types ( CInt )

import Input
import Types ()

import Control.Concurrent ( newMVar, swapMVar) --, threadDelay )

import Control.Lens

import qualified Debug.Trace as Tr 

newtype Size = Size (V2 CInt)
  
data Object = Object {
                           _pos :: V2 CInt
                        ,  _size :: Size
                        , _alive :: Bool
                        }

makeLenses ''Object

data Resources = Resources {_objectSprite :: SDL.Sprite.Sprite }
makeLenses ''Resources
  
data Objects = Objects {
    _player :: Object
  , _enemies :: [Object]
  }
makeLenses ''Objects
  
data GameState = GameState {
    _objects :: Objects
  }
makeLenses ''GameState

spritePath :: FilePath
spritePath = "data/testSprite.png"

initialGame =
  GameState
    (Objects
      (Object (V2 0 0) (Size (V2 500 500)) True)
      [(Object (V2 600 600) (Size (V2 500 500)) True)])

render :: SDL.Renderer -> Resources -> (GameState, Bool) -> IO Bool
render renderer (Resources sprite) (GameState (Objects (Object a _ alive0) (x:_)), exit) =
  do
    -- threadDelay 100000

    rendererDrawColor renderer $= V4 0 0 100 255
    clear renderer 

    case alive0 of
      True ->
        SDL.Sprite.render sprite a
      False ->
        pure ()

    SDL.Sprite.render sprite (x ^. pos)
    present renderer

    return exit

collides :: (V2 CInt, Size) -> (V2 CInt, Size) -> Bool
collides
  -- Box1
  (V2 x0 y0, Size (V2 sizeX0 sizeY0))
  -- Box2
  (V2 x1 y1, Size (V2 sizeX1 sizeY1)) = 
    x0 < x1 + sizeX1 && 
    x0 + sizeX0 > x1 && 
    y0 < y1 + sizeY1 && 
    sizeY0 + y0 > y1

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

-- objectGravity :: Double
objectGravity = V2 0 100

movingObject' :: V2 Double -> Y.SF Bool (V2 CInt, Y.Event Bool)
movingObject' initialPos = proc input -> do
  v <- Y.integralFrom initialPos -< objectGravity
  p <- Y.integralFrom 0 -< v
  returnA -< (fmap floor p, case input of
                              True -> Y.Event True            
                              False -> Y.NoEvent)

fallingObject :: V2 Double -> Y.SF Bool (V2 CInt)
fallingObject initialPos = 
  B.switch (Y.iPre False >>> movingObject' initialPos) (\_ -> fallingObject (V2 1 100))

movingObject :: V2 Double -> Y.SF (V2 CInt) (V2 CInt)
movingObject initialPos = proc move -> do
  -- Stop moving or something if a collision is detected
  p <- Y.integralFrom initialPos -< objectSpeed * fmap fromIntegral move 
  returnA -< fmap floor p

applyInputs :: GameState -> Y.SF InputState GameState
applyInputs initialGameState = proc input -> do
  -- Calculate new pos without modifying state
  objPos <- fallingObject 0 -< (input ^. (up . pressed))
  -- objPos <- movingObject (fmap fromIntegral (initialGameState ^. (objects . player . pos))) -< vectorizeMovement input
  let playerSize = initialGameState ^. (objects . player . size)

  enemyPos <- movingObject (fmap fromIntegral (initialGameState ^. (objects . enemies . to head . pos))) -< (V2 0 (-1))
  let enemySize = initialGameState ^. (objects . enemies . to head . size)
  
  isPlayerAlive <- checkCollisions -< ((objPos, (Size (V2 500 500))), (enemyPos, enemySize))

  let objectState = (Objects
                      -- Player
                      (Object objPos playerSize isPlayerAlive)
                      -- Test
                      [(Object enemyPos enemySize True)])

  returnA -< GameState objectState

update :: GameState -> Y.SF (Y.Event [SDL.Event]) (GameState, Bool)
update origGameState = proc events -> do
  newInputState <- accumHoldBy inputStateUpdate keybinds -< events

  gameState <- applyInputs origGameState -< newInputState

  returnA -< (gameState,
                 newInputState ^. (Input.quit . pressed))

main :: IO ()
main = do
  initializeAll

  window     <- createWindow (fromString "My SDL Application") defaultWindow
  renderer   <- createRenderer window (-1) defaultRenderer
  spritetest <- SDL.Sprite.load renderer spritePath (V2 500 500)

  let resources = Resources spritetest

  lastInteraction <- newMVar =<< SDL.time

  let senseInput _canBlock = do
                               currentTime <- SDL.time
                               dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
                               test <- Y.Event <$> SDL.pollEvents
                               return (dt, Just test)

  reactimate (return Y.NoEvent) senseInput (\ _ -> render renderer resources) (update initialGame)
  
  destroyRenderer renderer
  destroyWindow window
