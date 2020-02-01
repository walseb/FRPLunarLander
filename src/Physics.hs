module Physics where

import Linear
import YampaUtils.Types ()

objectGravity :: (RealFloat a) => V2 a
objectGravity = V2 0 (-300)
