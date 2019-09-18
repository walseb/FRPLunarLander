{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Main where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( forM_
                                                , guard
                                                , void
                                                )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , runReaderT
                                                )
import           Reflex
import           Reflex.SDL2
import qualified SDL                           as S
import qualified SDL.Sprite                    as Si


--------------------------------------------------------------------------------
-- | Renders an AABB using the handy SDL 2d 'Renderer'.
renderAABB :: MonadIO m => Renderer -> V4 Int -> V2 Int -> m ()
renderAABB r color pos = do
  rendererDrawColor r $= (fromIntegral <$> color)
  fillRect r $ Just $ Rectangle (P $ fromIntegral <$> pos - 10) 20


-------------------------------------------------------------------------------
-- | A type representing one layer in our app.
type Layer m = Performable m ()


----------------------------------------------------------------------
-- | Commit a layer stack that changes over time.
commitLayers
  :: (ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m)
  => Dynamic t [Layer m]
  -> m ()
commitLayers = tellDyn


----------------------------------------------------------------------
-- | Commit one layer that changes over time.
commitLayer
  :: (ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m)
  => Dynamic t (Layer m)
  -> m ()
commitLayer = tellDyn . fmap pure


ffor2 :: Reflex t => Dynamic t a -> Dynamic t b -> (a -> b -> c) -> Dynamic t c
ffor2 a b f = zipDynWith f a b

ffor2up
  :: Reflex t => Dynamic t a -> Dynamic t b1 -> ((a, b1) -> b) -> Dynamic t b
ffor2up a b = ffor (zipDyn a b)



guest :: (ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m, MonadReader Renderer m) => m ()
guest = do
  -- Print some stuff after the network is built.
  evPB <- getPostBuild
  performEvent_ $ ffor evPB $ \() -> liftIO $ putStrLn "starting up..."

  ------------------------------------------------------------------------------
  -- Get a handle on our renderer
  ------------------------------------------------------------------------------

  r <- ask
  spritetest <- Si.load r "data/testSprite.png" (V2 10 10)

  let color = (V4 100 100 100 100) :: V4 Int

  evMouseMove <- getMouseMotionEvent
  dMoves      <- foldDyn (\x xs -> take 100 $ x : xs) [] evMouseMove

  commitLayer $ ffor dMoves $ \btns ->
    forM_ (reverse btns) $ \dat -> do
      Si.render spritetest (V2 200 200)

  -- let (AABB motion pos) = renderAABB r color pos
  --   BSDL.copyOn image Nothing (Just $ SDL.Rectangle (SDL.P (SDL.V2 200 200)) (SDL.V2 100 100)) $ fst <$> eRender


  ------------------------------------------------------------------------------
  -- Quit on a quit event
  ------------------------------------------------------------------------------
  evQuit <- getQuitEvent
  performEvent_ $ liftIO (putStrLn "bye!") <$ evQuit
  shutdownOn =<< delay 0 evQuit


app :: (ReflexSDL2 t m, MonadReader Renderer m) => m ()
app = do
  (_, dynLayers) <- runDynamicWriterT guest
  r              <- ask
  performEvent_ $ ffor (updated dynLayers) $ \layers -> do
    rendererDrawColor r $= V4 0 0 0 255
    clear r
    sequence_ layers
    present r


main :: IO ()
main = do
  initializeAll
  let ogl = defaultOpenGL { glProfile = Core Debug 3 3 }
      cfg = defaultWindow { windowOpenGL      = Just ogl
                          , windowResizable   = True
                          , windowHighDPI     = False
                          , windowInitialSize = V2 640 480
                          }
  window <- createWindow "reflex-sdl2-exe" cfg
  void $ glCreateContext window

  putStrLn "creating renderer..."
  r <- createRenderer window (-1) defaultRenderer
  rendererDrawBlendMode r $= BlendAlphaBlend
  -- Host the network with an example of how to embed your own effects.
  -- In this case it's a simple reader.
  host $ runReaderT app r
  destroyRenderer r
  destroyWindow window
  quit
