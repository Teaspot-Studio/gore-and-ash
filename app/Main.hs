module Main where  
   
import Control.Monad (unless)
import Graphics

main :: IO ()
main = runWindow $ do
  rs <- initResources
  gameLoop rs 
  where 
    gameLoop rs = do 
      rs' <- stepRenderState rs
      unless (isClosedRequest rs') $ gameLoop rs' 