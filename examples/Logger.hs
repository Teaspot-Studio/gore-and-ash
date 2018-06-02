module Main where

import Game.GoreAndAsh
import Logger.API

-- | Application monad that is used for implementation of game API
type AppMonad = LoggerT Spider GMSpider

-- The application should be generic in the host monad that is used
app :: LoggerMonad t m => m ()
app = do
  msgE <- inputMessage
  outputMessage $ fmap (\msg -> "You said: " ++ msg) msgE

main :: IO ()
main = runGM $ runLoggerT (app :: AppMonad ())
