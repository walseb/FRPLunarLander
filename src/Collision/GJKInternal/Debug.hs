module Collision.GJKInternal.Debug where

import Collision.GJKInternal.Util
import Control.Lens
import Foreign.C.Types
import Linear
import qualified SDL.Vect as SV
import qualified Sprite as SP
import Types

debugRenderHitbox sprite pos size rot =
  let test = (toPt pos size rot)
      test2 = fmap (\a -> fmap floor a) test
   in mapM (debugRenderThing sprite (V2 20 20) 0) test2

debugRenderThing :: SP.Sprite -> V2 CInt -> CDouble -> V2 CInt -> IO ()
debugRenderThing sprite size rot newPosition =
  SP.renderEx
    sprite
    newPosition
    Nothing
    size
    rot
    (Just (SV.P (V2 0 0)))
    (V2 False False)

debugHitboxes :: GameState -> SP.Sprite -> IO [()]
debugHitboxes (GameState (Objects player objects)) sprite =
  debugRenderHitbox sprite (player ^. pos) (V2 500 500) (player ^. rot)
    >> debugRenderHitbox sprite (objects ^. (to head . pos)) (V2 500 500) (objects ^. (to head . rot))
