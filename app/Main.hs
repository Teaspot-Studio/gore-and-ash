module Main where  
   
import Control.Monad (unless)
import Data.Functor.Identity
import Game.GoreAndAsh
import Graphics

mainWire :: GameWire Identity a ()
mainWire = pure ()

main :: IO ()
main = runWindow $ do
  rs <- initResources
  gs <- newGameState mainWire
  gameLoop rs gs
  where 
    gameLoop rs gs = do 
      (_, gs') <- stepGame gs 
      rs' <- stepRenderState rs
      unless (isClosedRequest rs') $ gameLoop rs' gs'