import FRP.BearRiver as B
import FRP.Yampa as Y
import Control.Concurrent

import qualified SDL 
import qualified SDL.Sprite

import Data.String ( fromString )

import Foreign.C.Types ( CInt )

import Data.Coerce

import qualified Debug.Trace as D
  
type Pos = Double
type Vel = Double

fallingBall :: Pos -> Y.SF () (Pos, Vel)
fallingBall y0 = (constant (-9.81) >>> integral) >>> ((integral >>^ (+ y0)) &&& identity)

spritePath = "data/testSprite.png"

main :: IO ()
main = do
  SDL.initializeAll

  window     <- SDL.createWindow (fromString "My SDL Application") SDL.defaultWindow
  renderer   <- SDL.createRenderer window (-1) SDL.defaultRenderer
  spritetest <- SDL.Sprite.load renderer spritePath (SDL.V2 500 500)

  reactimate (return ())
                  (\ _ -> threadDelay 100000 >> return (0.1, Nothing))
                  (\ _ -> render renderer spritetest)
                  (fallingBall (10.0 :: Double))
  
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

render :: SDL.Renderer -> SDL.Sprite.Sprite -> (Pos, Vel) -> IO Bool
render renderer sprite (pos, vel) =
  do
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 100 255
    SDL.clear renderer 
    SDL.Sprite.render sprite (SDL.V2 0 0)
    SDL.present renderer
    return False
