module FPS(
    makeFPSBounder
  , waitFPSBound
  ) where

import GHC.Event
import Control.Concurrent
import Control.Monad 

type FPSBound = MVar ()

-- | Creates mvar that fills periodically with given fps
makeFPSBounder :: Int -> IO FPSBound
makeFPSBounder fps = do
  v <- newEmptyMVar 
  tm <- getSystemTimerManager
  let t = ceiling ((1000000 :: Double) / fromIntegral fps)
  callback v tm t
  return v
  where
    callback v tm t = do 
      putMVar v ()
      void $ registerTimeout tm t $ callback v tm t

-- | Wait until next FPS value is reached when the function unblocks
waitFPSBound :: FPSBound -> IO ()
waitFPSBound = takeMVar 