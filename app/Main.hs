module Main where  
   
import Control.Monad (unless)
import Game 
import Game.GoreAndAsh
import Graphics
import Render 

main :: IO ()
main = runWindow $ do
  rs <- initResources
  gs <- newGameState mainWire
  gameLoop rs gs
  where 
    gameLoop rs gs = do 
      (mg, gs') <- stepGame gs 
      rs2 <- stepRenderState rs
      let 
        rs3 = case mg of 
          Nothing -> rs2
          Just g -> renderGame g rs2 
      unless (isClosedRequest rs3) $ gameLoop rs3 gs'
