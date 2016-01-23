{-|
Module      : Game.GoreAndAsh.Core.State
Description : Core operations with main application loop.
Copyright   : (c) Anton Gushcha, 2015-2016
                  Oganyan Levon, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

Handling of game main loop, creation of initial state, stepping and cleaning up.
-}
module Game.GoreAndAsh.Core.State(
    GameState(..)
  , stepGame
  , newGameState
  , newGameStateM
  , cleanupGameState
  ) where

import Prelude hiding (id, (.))
import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Wire
import Game.GoreAndAsh.Core.Arrow
import Game.GoreAndAsh.Core.Monad 
import Game.GoreAndAsh.Core.Session 

-- | Holds all data that is needed to produce next step
-- of game simulation. 
--
-- You need to call 'stepGame' to get next game state repeatedly 
-- and finally 'cleanupGameState' at the end of program.
--
-- [@m@] is game monad is used including all enabled API of core modules;
--
-- [@s@] is game state that includes chained state of core modules;
--
-- [@a@] is return value of main arrow;
--
-- Typical game main loop:
-- 
-- @
-- main :: IO ()
-- main = withModule (Proxy :: Proxy AppMonad) $ do
--   gs <- newGameState $ runActor' mainWire
--   gsRef <- newIORef gs
--   firstStep gs gsRef `onCtrlC` exitHandler gsRef
--   where
--     -- | What to do on emergency exit
--     exitHandler gsRef = do 
--       gs <- readIORef gsRef 
--       cleanupGameState gs
--       exitSuccess
-- 
--     -- | Initialization step
--     firstStep gs gsRef = do 
--       (_, gs') <- stepGame gs $ do 
--         -- ... some initialization steps
--       writeIORef gsRef gs'
--       gameLoop gs' gsRef
-- 
--     -- | Normal game loop
--     gameLoop gs gsRef = do 
--       (_, gs') <- stepGame gs (return ())
--       writeIORef gsRef gs'
--       gameLoop gs' gsRef
-- 
-- -- | Executes given handler on Ctrl-C pressing
-- onCtrlC :: IO a -> IO () -> IO a
-- p `onCtrlC` q = catchJust isUserInterrupt p (const $ q >> p `onCtrlC` q)
--   where
--     isUserInterrupt :: AsyncException -> Maybe ()
--     isUserInterrupt UserInterrupt = Just ()
--     isUserInterrupt _             = Nothing
-- @
data GameState m s a = GameState {
  gameSession :: !GameSession 
, gameWire :: !(GameWire m () a)
, gameContext :: !GameContext
, gameModuleState :: !s
}

instance NFData s => NFData (GameState m s a) where
  rnf GameState{..} = gameSession `seq`
    gameWire `seq`
    gameContext `deepseq`
    gameModuleState `deepseq` ()
    
-- | Main loop of the game where each frame is calculated.
--
-- Call it frequently enough for smooth simulation. At the end
-- of application there should be call to 'cleanupGameState'.
stepGame :: (GameModule m s, NFData s, MonadIO m') 
  => GameState m s a -- ^ Current game state
  -> GameMonadT m b -- ^ Some action to perform before each frame
  -> m' (Maybe a, GameState m s a)
  -- ^ Main wire can inhibit therefore result is 'Maybe'
stepGame GameState{..} preFrame = do 
  (t, gameSession') <- stepGameSession gameSession
  -- Removing layers of abstraction
  let gameMonadAction = stepWire gameWire t $ Right ()
      moduleAction = evalGameMonad (preFrame >> gameMonadAction) gameContext
      ioAction = runModule moduleAction gameModuleState
  -- Final pattern matching
  (((ma, gameWire'), gameContext'), gameModuleState') <- ioAction
  -- Collect new state
  let newState = GameState {
      gameSession = gameSession'
    , gameWire = gameWire'
    , gameContext = gameContext'
    , gameModuleState = gameModuleState'
    }
  return $ gameModuleState' 
    `deepseq` gameContext'
    `deepseq` (eitherToMaybe ma, newState)

-- | Helper to throw away left value
eitherToMaybe :: Either a b -> Maybe b 
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right a) = Just a

-- | Creates new game state from given main wire.
--
-- Use 'stepGame' to update the state and free it with
-- 'cleanupGameState' at the end of your application. 
--
-- If you need some initialization steps, you can use
-- 'newGameStateM' version.
newGameState :: (GameModule m s, MonadIO m') => 
     GameWire m () a -- ^ Wire that we calculate
  -> m' (GameState m s a)
newGameState wire = do 
  moduleState <- newModuleState
  return $ GameState {
      gameSession = newGameSession
    , gameWire = wire 
    , gameContext = newGameContext
    , gameModuleState = moduleState
    }

-- | Creates new game state, monadic version that allows some
-- initialization steps in game monad.
--
-- The function is helpful if you want to make an global actor from
-- your main wire.
--
-- Use 'stepGame' to update the state and free it with
-- 'cleanupGameState' at the end of your application. 
--
-- See also 'newGameState'.
newGameStateM :: (GameModule m s, MonadIO m') => 
    GameMonadT m (GameWire m () a) -- ^ Action that makes wire to execute
  -> m' (GameState m s a)
newGameStateM mwire = do 
  moduleState <- newModuleState
  let moduleAction = evalGameMonad mwire newGameContext
      ioAction = runModule moduleAction moduleState
  ((wire, gameContext'), moduleState') <- ioAction
  return $! GameState {
      gameSession = newGameSession
    , gameWire = wire
    , gameContext = gameContext'
    , gameModuleState = moduleState'
    }

-- | Cleanups resources that is holded in game state.
--
-- The function should be called before the exit of application to
-- free all resources catched by core modules.
cleanupGameState :: (GameModule m s, MonadIO m') 
  => GameState m s a -- ^ Game state with resources
  -> m' ()
cleanupGameState = liftIO . cleanupModule . gameModuleState