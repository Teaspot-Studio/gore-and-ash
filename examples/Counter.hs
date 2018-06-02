module Main where

import Game.GoreAndAsh
import Game.GoreAndAsh.Time

import Counter.API
import Logger.API

-- | Application monad that is used for implementation of game API
type AppMonad = LoggerT Spider (CounterT Spider GMSpider)

-- The application should be generic in the host monad that is used
app :: (LoggerMonad t m, CounterMonad t m, MonadGame t m) => m ()
app = do
  -- Show value of counter every second
  timeE <- tickEvery (realToFrac (1 :: Double))
  cntDyn <- getCounter
  let msgE = tagPromptlyDyn cntDyn timeE
  outputMessage $ ffor msgE $ \v -> "Counter: " ++ show v

  -- Update value of counter every 2 seconds
  updE <- tickEvery (realToFrac (2 :: Double))
  incCounter updE

  -- Capture old value of counter
  oldMsgE <- delay 0 msgE
  outputMessage $ ffor oldMsgE $ \v -> "Counter was: " ++ show v

main :: IO ()
main = runGM $ runCounterT $ runLoggerT (app :: AppMonad ())
