module Game.GoreAndAsh.Input.GLFW.API(
  -- | Arrow API
    keyStatus
  , keyStatusDyn
  ) where

import Graphics.UI.GLFW
import Control.Wire.Unsafe.Event

import Game.GoreAndAsh
import Game.GoreAndAsh.Input.GLFW.Module 

-- | Produces event when key state changes
keyStatus :: MonadGLFWInput m => Key -> GameWire m a (Event (KeyState, ModifierKeys))
keyStatus k = liftGameMonad (maybe2Event <$> keyStatusM k)

-- | Produces event when key state changes, get key as arrow argument
keyStatusDyn :: MonadGLFWInput m => GameWire m Key (Event (KeyState, ModifierKeys))
keyStatusDyn = liftGameMonad1 $ \k -> do 
  ms <- keyStatusM k 
  return $ maybe2Event ms 

-- | Simple transform from maybe to event
maybe2Event :: Maybe a -> Event a 
maybe2Event Nothing = NoEvent 
maybe2Event (Just a) = Event a