{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.SDL.Module(
    SDLT(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Fix 
import Control.Monad.IO.Class 
import Control.Monad.State.Strict
import qualified Data.Foldable as F 
import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S 

import SDL

import Game.GoreAndAsh
import Game.GoreAndAsh.SDL.State

newtype SDLT s m a = SDLT { runSDLT :: StateT (SDLState s) m a }
  deriving (Functor, Applicative, Monad, MonadState (SDLState s), MonadFix, MonadTrans, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance GameModule m s => GameModule (SDLT s m) (SDLState s) where 
  type ModuleState (SDLT s m) = SDLState s
  runModule (SDLT m) s = do
    s' <- processEvents s 
    clearWindows s'
    ((a, s''), nextState) <- runModule (runStateT m s') (sdlNextState s')
    drawWindows s''
    return (a, flashSDLState $ s'' { 
        sdlNextState = nextState
      })

  newModuleState = emptySDLState <$> newModuleState
  withModule _ io = initializeAll >> io
  cleanupModule _ = quit

-- | Takes all window and renderers and update them
drawWindows :: MonadIO m => SDLState s -> m ()
drawWindows SDLState{..} = mapM_ go . H.elems $! sdlWindows
  where 
  go (_, r, _) = present r 

-- | Clear surface of all windows
clearWindows :: MonadIO m => SDLState s -> m ()
clearWindows SDLState{..} = mapM_ go . H.elems $! sdlWindows
  where 
  go (_, r, c) = do 
    rendererDrawColor r $= c
    clear r

-- | Catch all SDL events
processEvents :: MonadIO m => SDLState s -> m (SDLState s)
processEvents sdlState = do 
  es <- pollEvents
  return $! F.foldl' process sdlState (eventPayload <$> es)
  where 
  process s e = case e of 
    WindowShownEvent d -> s { sdlWindowShownEvents = sdlWindowShownEvents s S.|> d }
    WindowHiddenEvent d -> s { sdlWindowHiddenEvents = sdlWindowHiddenEvents s S.|> d }
    WindowExposedEvent d -> s { sdlWindowExposedEvents = sdlWindowExposedEvents s S.|> d }
    WindowMovedEvent d -> s { sdlWindowMovedEvents = sdlWindowMovedEvents s S.|> d }
    WindowResizedEvent d -> s { sdlWindowResizedEvents = sdlWindowResizedEvents s S.|> d }
    WindowSizeChangedEvent d -> s { sdlWindowSizeChangedEvents = sdlWindowSizeChangedEvents s S.|> d }
    WindowMinimizedEvent d -> s { sdlWindowMinimizedEvents = sdlWindowMinimizedEvents s S.|> d }
    WindowMaximizedEvent d -> s { sdlWindowMaximizedEvents = sdlWindowMaximizedEvents s S.|> d }
    WindowRestoredEvent d -> s { sdlWindowRestoredEvents = sdlWindowRestoredEvents s S.|> d }
    WindowGainedMouseFocusEvent d -> s { sdlWindowGainedMouseFocusEvents = sdlWindowGainedMouseFocusEvents s S.|> d }
    WindowLostMouseFocusEvent d -> s { sdlWindowLostMouseFocusEvents = sdlWindowLostMouseFocusEvents s S.|> d }
    WindowGainedKeyboardFocusEvent d -> s { sdlWindowGainedKeyboardFocusEvents = sdlWindowGainedKeyboardFocusEvents s S.|> d }
    WindowLostKeyboardFocusEvent d -> s { sdlWindowLostKeyboardFocusEvents = sdlWindowLostKeyboardFocusEvents s S.|> d }
    WindowClosedEvent d -> s { sdlWindowClosedEvents = sdlWindowClosedEvents s S.|> d }
    KeyboardEvent d -> s { sdlKeyboardEvents = sdlKeyboardEvents s S.|> d }
    TextEditingEvent d -> s { sdlTextEditingEvents = sdlTextEditingEvents s S.|> d }
    TextInputEvent d -> s { sdlTextInputEvents = sdlTextInputEvents s S.|> d }
    MouseMotionEvent d -> s { sdlMouseMotionEvents = sdlMouseMotionEvents s S.|> d }
    MouseButtonEvent d -> s { sdlMouseButtonEvents = sdlMouseButtonEvents s S.|> d }
    MouseWheelEvent d -> s { sdlMouseWheelEvents = sdlMouseWheelEvents s S.|> d }
    JoyAxisEvent d -> s { sdlJoyAxisEvents = sdlJoyAxisEvents s S.|> d }
    JoyBallEvent d -> s { sdlJoyBallEvents = sdlJoyBallEvents s S.|> d }
    JoyHatEvent d -> s { sdlJoyHatEvents = sdlJoyHatEvents s S.|> d }
    JoyButtonEvent d -> s { sdlJoyButtonEvents = sdlJoyButtonEvents s S.|> d }
    JoyDeviceEvent d -> s { sdlJoyDeviceEvents = sdlJoyDeviceEvents s S.|> d }
    ControllerAxisEvent d -> s { sdlControllerAxisEvents = sdlControllerAxisEvents s S.|> d }
    ControllerButtonEvent d -> s { sdlControllerButtonEvents = sdlControllerButtonEvents s S.|> d }
    ControllerDeviceEvent d -> s { sdlControllerDeviceEvents = sdlControllerDeviceEvents s S.|> d }
    QuitEvent -> s { sdlQuitEvent = True }
    UserEvent d -> s { sdlUserEvents = sdlUserEvents s S.|> d }
    SysWMEvent d -> s { sdlSysWMEvents = sdlSysWMEvents s S.|> d }
    TouchFingerEvent d -> s { sdlTouchFingerEvents = sdlTouchFingerEvents s S.|> d }
    MultiGestureEvent d -> s { sdlMultiGestureEvents = sdlMultiGestureEvents s S.|> d }
    DollarGestureEvent d -> s { sdlDollarGestureEvents = sdlDollarGestureEvents s S.|> d }
    DropEvent d -> s { sdlDropEvents = sdlDropEvents s S.|> d }
    ClipboardUpdateEvent d -> s { sdlClipboardUpdateEvents = sdlClipboardUpdateEvents s S.|> d }
    _ -> s