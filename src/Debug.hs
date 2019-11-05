module Debug where

import Control.Lens
import Linear
import qualified Sprite as SP
import YampaUtils.Types ()
import Types
import Collision.GJK
  ( Size (..),
    debugRenderHitbox,
  )

debugHitboxes :: GameState -> SP.Sprite -> IO [()]
debugHitboxes (GameState (Objects player objects)) sprite =
  debugRenderHitbox sprite (player ^. pos) (Size (V2 500 500)) (player ^. rot)
    >> debugRenderHitbox sprite (objects ^. (to head . pos)) (Size (V2 500 500)) (objects ^. (to head . rot))
