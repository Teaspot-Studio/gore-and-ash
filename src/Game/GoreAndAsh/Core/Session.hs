module Game.GoreAndAsh.Core.Session(
    GameTime
  , GameSession
  , newGameSession
  , stepGameSession
  ) where

import Control.Monad.IO.Class
import Control.Wire (NominalDiffTime)
import Control.Wire.Session

-- | Current value of simulation time
type GameTime = Timed NominalDiffTime ()

-- | Session that stores time in diff format
-- The only purpose is to store time while stepping simulation
type GameSession = Session IO GameTime

-- | Creates new empty game session
newGameSession :: GameSession
newGameSession = clockSession_

-- | Generates next value of game session and outputs current simulation time
-- That simulation time should be feeded to game wire and next
-- value of session should be used at next step of simulation.
stepGameSession :: MonadIO m => GameSession -> m (GameTime, GameSession)
stepGameSession s = liftIO $ stepSession s