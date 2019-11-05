{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Foreign.C.Types
import Linear
import qualified Sprite as SP
import YampaUtils.Types ()

newtype Size = Size (V2 CInt)

data Object
  = Object
      { _pos :: V2 CInt,
        _size :: Size,
        _rot :: Double,
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
