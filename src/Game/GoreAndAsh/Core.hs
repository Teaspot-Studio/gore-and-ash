{-|
Module      : Game.GoreAndAsh.Core
Description : Engine Core that controls modules execution
Copyright   : (c) Anton Gushcha, 2015-2016
                  Oganyan Levon, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The core of all engine. It contains generic reactive operations and helpers,
definition of core module system and utilities to control main loop of
application.
-}
module Game.GoreAndAsh.Core(
    module Game.GoreAndAsh.Core.Delay
  , module Game.GoreAndAsh.Core.ExternalRef
  , module Game.GoreAndAsh.Core.Monad
  ) where

import Game.GoreAndAsh.Core.Delay
import Game.GoreAndAsh.Core.ExternalRef
import Game.GoreAndAsh.Core.Monad