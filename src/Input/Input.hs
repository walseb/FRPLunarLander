module Input.Input where

import SDL
import Input.Internal
import Input.Types

inputStateUpdate :: InputState -> [SDL.Event] -> InputState
inputStateUpdate input events =
  foldr updateKeyInInputState input (eventToCustomEventPayload events)
