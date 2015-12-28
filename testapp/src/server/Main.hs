module Main where 

import Control.DeepSeq 
import Control.Exception
import Data.Proxy 
import Game 
import Game.GoreAndAsh
import System.Exit

main :: IO ()
main = withModule (Proxy :: Proxy AppMonad) $ do
  gs <- newGameState mainWire
  gameLoop gs `onCtrlC` (cleanupGameState gs >> exitSuccess)
  where
    gameLoop gs = do 
      (mg, gs') <- stepGame gs (return ())
      mg `deepseq` gs' `deepseq` gameLoop gs'

onCtrlC :: IO a -> IO () -> IO a
p `onCtrlC` q = catchJust isUserInterrupt p (const $ q >> p `onCtrlC` q)
  where
    isUserInterrupt :: AsyncException -> Maybe ()
    isUserInterrupt UserInterrupt = Just ()
    isUserInterrupt _             = Nothing