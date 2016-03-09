{-|
Module      : Game.GoreAndAsh.Core
Description : Engine Core that controls modules execution
Copyright   : (c) Anton Gushcha, 2015-2016
                  Oganyan Levon, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

The core of all engine. It contains generic arrow operations and helpers,
definition of core module system, game session declaration and utilities
to control main loop of application.
-}
module Game.GoreAndAsh.Core(
  -- * Reexports of used time types
    GameTime
  , GameSession
  , NominalDiffTime
  -- * Game loop control
  , GameState
  , stepGame
  , newGameState
  , newGameStateM
  , cleanupGameState
  -- * Core module definition
  , GameMonadT
  , GameModule(..)
  , ModuleStack
  -- * Arrow combinators and helpers
  , GameWire
  -- ** Lifting monad to arrow
  , liftGameMonad
  , liftGameMonad1
  , liftGameMonad2
  , liftGameMonad3
  , liftGameMonad4
  , liftGameMonadOnce
  , liftGameMonad1Once
  , liftGameMonad2Once
  , liftGameMonad3Once
  , liftGameMonad4Once
  -- ** Event functions
  , once'
  , mapE
  , filterE
  , filterEG
  , filterEGM
  , filterJustE
  , filterJustLE
  , liftGameMonadEvent1
  , changes
  -- ** Helpers
  , stateWire
  , chainWires
  , dispense
  , dDispense
  , withInit
  , nothingInhibit
  -- ** Time utilities
  , deltaTime
  ) where

import Game.GoreAndAsh.Core.Arrow as X
import Game.GoreAndAsh.Core.Monad as X
import Game.GoreAndAsh.Core.Session as X
import Game.GoreAndAsh.Core.State as X
