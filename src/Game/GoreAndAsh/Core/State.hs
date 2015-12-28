module Game.GoreAndAsh.Core.State(
    GameState(..)
  , stepGame
  , newGameState
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
-- of game simulation
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
    
-- | Main loop of the game where each frame is calculated
stepGame :: (GameModule m s, NFData s, MonadIO m') 
  => GameState m s a -- ^ Current game state
  -> GameMonadT m b -- ^ Some action to perform before frame
  -> m' (Maybe a, GameState m s a)
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

-- | Creates new game state
-- Use stepGame to execute 
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

-- | Cleanups resources that is holded in game state
cleanupGameState :: (GameModule m s, MonadIO m') 
  => GameState m s a -- ^ Game state with resources
  -> m' ()
cleanupGameState = liftIO . cleanupModule . gameModuleState