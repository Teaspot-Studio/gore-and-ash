module Main where

import Game.GoreAndAsh

import Counter.API
import Logger.API
import Timer.API

-- | Application monad that is used for implementation of game API
type AppMonad = LoggerT Spider (TimerT Spider (CounterT Spider (GameMonad Spider)))

-- The application should be generic in the host monad that is used
app :: (LoggerMonad t m, TimerMonad t m, CounterMonad t m) => m ()
app = do
  -- Show value of counter every second
  timeE <- tickEvery (realToFrac (1 :: Double))
  cntDyn <- getCounter
  let msgE = tagPromptlyDyn cntDyn timeE
  outputMessage $ ffor msgE $ \v -> "Counter: " ++ show v

  -- Update value of counter every 2 seconds
  updE <- tickEvery (realToFrac (2 :: Double))
  incCounter updE

main :: IO ()
main = runSpiderHost $ hostApp $ runModule () (app :: AppMonad ())