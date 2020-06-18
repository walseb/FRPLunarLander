module Input where

import FRPEngine.Input.Utils (vectorizeMovement)
import FRPEngine.Input.Types
import SDL

keyBinds =
  [
    InpScroll 0,
    InpBtn4D
      ( V4
          (Btn KeycodeM False)
          (Btn KeycodeT False)
          (Btn KeycodeS False)
          (Btn KeycodeN False)
      ),
    InpCloseWindow False
  ]

scrollKey = (!! 0)

moveKey keys = vectorizeMovement (keys !! 1)

quitKey keys = case (keys !! 2) of
  (InpCloseWindow True) -> True
  (InpCloseWindow False) -> False
  _ -> error "Quit key not implemented"
