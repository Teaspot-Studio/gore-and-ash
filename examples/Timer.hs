module Main where

import Game.GoreAndAsh
import Game.GoreAndAsh.Time
import Logger.API

-- | Application monad that is used for implementation of game API
type AppMonad = LoggerT Spider GMSpider

-- The application should be generic in the host monad that is used
app :: (LoggerMonad t m, MonadGame t m) => m ()
app = do
  timeE <- tickEvery (realToFrac (2 :: Double))
  outputMessage $ ffor timeE $ const "Tick!"

main :: IO ()
main = runGM $ runLoggerT (app :: AppMonad ())
