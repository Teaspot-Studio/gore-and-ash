module Main where

import Game.GoreAndAsh
import Timer.API
import Logger.API

-- | Application monad that is used for implementation of game API
type AppMonad = LoggerT Spider (TimerT Spider (GameMonad Spider))

-- The application should be generic in the host monad that is used
app :: (LoggerMonad t m, TimerMonad t m) => m ()
app = do
  timeE <- tickEvery (realToFrac (2 :: Double))
  outputMessage $ ffor timeE $ const "Tick!"

main :: IO ()
main = runSpiderHost $ hostApp $ runModule () (app :: AppMonad ())