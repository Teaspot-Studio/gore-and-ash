module Main where  

import Data.Proxy 
import Game 
import Game.GoreAndAsh
import Game.GoreAndAsh.GLFW
import Game.GoreAndAsh.Network
import Graphics
import Render 

main :: IO ()
main = withModule (Proxy :: Proxy AppMonad) $ runWindow $ do
  rs <- initResources
  gs <- newGameState mainWire
  firstLoop rs gs 
  where 
    firstLoop rs gs = do 
      (_, gs') <- stepGame gs $ do 
        networkBind Nothing 1 2 0 0
      gameLoop rs gs'

    gameLoop rs gs = do 
      (mg, gs') <- stepGame gs (preFrame rs)
      rs2 <- stepRenderState rs
      let 
        rs3 = case mg of 
          Nothing -> rs2
          Just g -> renderGame g rs2
      if isClosedRequest rs3 
        then cleanupGameState gs'
        else gameLoop rs3 gs'

    preFrame rs = do 
      setCurrentWindowM $ renderWindow rs