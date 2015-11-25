module Game.GoreAndAsh.Core.Arrow(
    GameSession
  , GameWire
  , GameTime
  ) where

import Control.Wire.Core
import Game.GoreAndAsh.Core.Monad
import Game.GoreAndAsh.Core.Session

-- | Game wire with given API @m@ and input value @a@ and output value @b@
type GameWire m a b = Wire GameTime () (GameMonadT m) a b

