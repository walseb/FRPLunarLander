{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}

import FRP.BearRiver as B
import FRP.Yampa as Y

import SDL
import qualified SDL.Sprite

import Data.String ( fromString )

import Foreign.C.Types ( CInt )

import Input

-- newtype Player = Player { _location :: V2 CInt }

-- data GameState = GameState {_player :: Player}

-- makeLenses ''Player

spritePath :: FilePath
spritePath = "data/testSprite.png"

render :: SDL.Renderer -> SDL.Sprite.Sprite -> (V2 CInt, Bool) -> IO Bool
render renderer sprite (a, exit) =
  do
    rendererDrawColor renderer $= V4 0 0 100 255
    clear renderer 
    SDL.Sprite.render sprite a
    present renderer
    return exit

update :: Y.SF (Y.Event [SDL.Event]) (V2 CInt, Bool)
update = proc events -> do
  updateInputState <- accumHoldBy inputStateUpdate keybinds -< events

  cubePos <- accumHoldBy (+) (V2 0 0) -< Y.Event (vectorizeMovement updateInputState)

  returnA -< (cubePos, (_pressed . _quit) updateInputState)
  
main :: IO ()
main = do
  initializeAll

  window     <- createWindow (fromString "My SDL Application") defaultWindow
  renderer   <- createRenderer window (-1) defaultRenderer
  spritetest <- SDL.Sprite.load renderer spritePath (V2 500 500)

  reactimate (return Y.NoEvent)
                  (\ _ -> do
                     -- threadDelay 100000
                     test <- Y.Event <$> SDL.pollEvents
                     return (0.1, Just test))
                  (\ _ -> render renderer spritetest)
                  update
  
  destroyRenderer renderer
  destroyWindow window
