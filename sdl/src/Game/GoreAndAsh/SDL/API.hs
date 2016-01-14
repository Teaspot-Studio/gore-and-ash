module Game.GoreAndAsh.SDL.API(
    SDLMonad(..)
  ) where

import Control.Monad.State.Strict
import Data.Sequence (Seq)
import Prelude hiding (id, (.))
import qualified Data.Sequence as S 

import SDL hiding (get)
import SDL.Event 

import Game.GoreAndAsh
import Game.GoreAndAsh.SDL.State
import Game.GoreAndAsh.SDL.Module

-- | Low level API for module
class MonadIO m => SDLMonad m where 

  -- | Getting window shown events that occurs scince last frame
  sdlWindowShownEventsM :: m (Seq WindowShownEventData)
  -- | Getting window hidden events that occurs scince last frame
  sdlWindowHiddenEventsM :: m (Seq WindowHiddenEventData)
  -- | Getting window exposed events that occurs scince last frame
  sdlWindowExposedEventsM :: m (Seq WindowExposedEventData)
  -- | Getting window move events that occurs scince last frame
  sdlWindowMovedEventsM :: m (Seq WindowMovedEventData)
  -- | Getting window resize events that occurs scince last frame
  -- 
  -- This is event is always preceded by WindowSizeChangedEvent.
  sdlWindowResizedEventsM :: m (Seq WindowResizedEventData)
  -- | Getting window resize events that occurs scince last frame
  --
  -- The window size has changed, either as a result of an API call or through the system or user changing the window size; this event is followed by WindowResizedEvent if the size was changed by an external event, i.e. the user or the window manager.
  sdlWindowSizeChangedEventsM :: m (Seq WindowSizeChangedEventData)
  -- | Getting window minimization events that occurs scince last frame
  sdlWindowMinimizedEventsM :: m (Seq WindowMinimizedEventData)
  -- | Getting window maximization events that occurs scince last frame
  sdlWindowMaximizedEventsM :: m (Seq WindowMaximizedEventData)
  -- | Getting window restore events that occurs scince last frame
  sdlWindowRestoredEventsM :: m (Seq WindowRestoredEventData)
  -- | Getting window focus events that occurs scince last frame
  sdlWindowGainedMouseFocusEventsM :: m (Seq WindowGainedMouseFocusEventData)
  -- | Getting window focus events that occurs scince last frame
  sdlWindowLostMouseFocusEventsM :: m (Seq WindowLostMouseFocusEventData)
  -- | Getting window focus events that occurs scince last frame
  sdlWindowGainedKeyboardFocusEventsM :: m (Seq WindowGainedKeyboardFocusEventData)
  -- | Getting window focus events that occurs scince last frame
  sdlWindowLostKeyboardFocusEventsM :: m (Seq WindowLostKeyboardFocusEventData)
  -- | Getting window close events that occurs scince last frame
  sdlWindowClosedEventsM :: m (Seq WindowClosedEventData)

  -- | Getting keyboard events that occurs scince last frame
  sdlKeyboardEventsM :: m (Seq KeyboardEventData)
  -- | Getting input API events that occurs scince last frame
  sdlTextEditingEventsM :: m (Seq TextEditingEventData)
  -- | Getting input API events that occurs scince last frame
  sdlTextInputEventsM :: m (Seq TextInputEventData)

  -- | Getting mouse events that occurs scince last frame
  sdlMouseMotionEventsM :: m (Seq MouseMotionEventData)
  -- | Getting mouse events that occurs scince last frame
  sdlMouseButtonEventsM :: m (Seq MouseButtonEventData)
  -- | Getting mouse events that occurs scince last frame
  sdlMouseWheelEventsM :: m (Seq MouseWheelEventData)

  -- | Getting joystick events that occurs scince last frame
  sdlJoyAxisEventsM :: m (Seq JoyAxisEventData)
  -- | Getting joystick events that occurs scince last frame
  sdlJoyBallEventsM :: m (Seq JoyBallEventData)
  -- | Getting joystick events that occurs scince last frame
  sdlJoyHatEventsM :: m (Seq JoyHatEventData)
  -- | Getting joystick events that occurs scince last frame
  sdlJoyButtonEventsM :: m (Seq JoyButtonEventData)
  -- | Getting joystick events that occurs scince last frame
  sdlJoyDeviceEventsM :: m (Seq JoyDeviceEventData)

  -- | Getting controller events that occurs scince last frame
  sdlControllerAxisEventsM :: m (Seq ControllerAxisEventData)
  -- | Getting controller events that occurs scince last frame
  sdlControllerButtonEventsM :: m (Seq ControllerButtonEventData)
  -- | Getting controller events that occurs scince last frame
  sdlControllerDeviceEventsM :: m (Seq ControllerDeviceEventData)

  -- | Getting quit request event
  sdlQuitEventM :: m Bool
  -- | Getting user events that occurs scince last frame
  sdlUserEventsM :: m (Seq UserEventData)
  -- | Getting video driver specific events that occurs scince last frame
  sdlSysWMEventsM :: m (Seq SysWMEventData)

  -- | Getting touch events that occurs scince last frame
  sdlTouchFingerEventsM :: m (Seq TouchFingerEventData)
  -- | Getting touch events that occurs scince last frame
  sdlMultiGestureEventsM :: m (Seq MultiGestureEventData)
  -- | Getting touch events that occurs scince last frame
  sdlDollarGestureEventsM :: m (Seq DollarGestureEventData)
  
  -- | Getting file opened events that occurs scince last frame
  sdlDropEventsM :: m (Seq DropEventData)
  -- | Getting clipboard changed events that occurs scince last frame
  sdlClipboardUpdateEventsM :: m (Seq ClipboardUpdateEventData)

instance {-# OVERLAPPING #-} MonadIO m => SDLMonad (SDLT s m) where
  sdlWindowShownEventsM = sdlWindowShownEvents <$> get
  sdlWindowHiddenEventsM = sdlWindowHiddenEvents <$> get
  sdlWindowExposedEventsM = sdlWindowExposedEvents <$> get
  sdlWindowMovedEventsM = sdlWindowMovedEvents <$> get
  sdlWindowResizedEventsM = sdlWindowResizedEvents <$> get
  sdlWindowSizeChangedEventsM = sdlWindowSizeChangedEvents <$> get
  sdlWindowMinimizedEventsM = sdlWindowMinimizedEvents <$> get
  sdlWindowMaximizedEventsM = sdlWindowMaximizedEvents <$> get
  sdlWindowRestoredEventsM = sdlWindowRestoredEvents <$> get
  sdlWindowGainedMouseFocusEventsM = sdlWindowGainedMouseFocusEvents <$> get
  sdlWindowLostMouseFocusEventsM = sdlWindowLostMouseFocusEvents <$> get
  sdlWindowGainedKeyboardFocusEventsM = sdlWindowGainedKeyboardFocusEvents <$> get
  sdlWindowLostKeyboardFocusEventsM = sdlWindowLostKeyboardFocusEvents <$> get
  sdlWindowClosedEventsM = sdlWindowClosedEvents <$> get
  sdlKeyboardEventsM = sdlKeyboardEvents <$> get
  sdlTextEditingEventsM = sdlTextEditingEvents <$> get
  sdlTextInputEventsM = sdlTextInputEvents <$> get
  sdlMouseMotionEventsM = sdlMouseMotionEvents <$> get
  sdlMouseButtonEventsM = sdlMouseButtonEvents <$> get
  sdlMouseWheelEventsM = sdlMouseWheelEvents <$> get
  sdlJoyAxisEventsM = sdlJoyAxisEvents <$> get
  sdlJoyBallEventsM = sdlJoyBallEvents <$> get
  sdlJoyHatEventsM = sdlJoyHatEvents <$> get
  sdlJoyButtonEventsM = sdlJoyButtonEvents <$> get
  sdlJoyDeviceEventsM = sdlJoyDeviceEvents <$> get
  sdlControllerAxisEventsM = sdlControllerAxisEvents <$> get
  sdlControllerButtonEventsM = sdlControllerButtonEvents <$> get
  sdlControllerDeviceEventsM = sdlControllerDeviceEvents <$> get
  sdlQuitEventM = sdlQuitEvent <$> get
  sdlUserEventsM = sdlUserEvents <$> get
  sdlSysWMEventsM = sdlSysWMEvents <$> get
  sdlTouchFingerEventsM = sdlTouchFingerEvents <$> get
  sdlMultiGestureEventsM = sdlMultiGestureEvents <$> get
  sdlDollarGestureEventsM = sdlDollarGestureEvents <$> get
  sdlDropEventsM = sdlDropEvents <$> get
  sdlClipboardUpdateEventsM = sdlClipboardUpdateEvents <$> get

instance {-# OVERLAPPABLE #-} (MonadIO (mt m), SDLMonad m, MonadTrans mt) => SDLMonad (mt m) where 
  sdlWindowShownEventsM = lift sdlWindowShownEventsM
  sdlWindowHiddenEventsM = lift sdlWindowHiddenEventsM
  sdlWindowExposedEventsM = lift sdlWindowExposedEventsM
  sdlWindowMovedEventsM = lift sdlWindowMovedEventsM
  sdlWindowResizedEventsM = lift sdlWindowResizedEventsM
  sdlWindowSizeChangedEventsM = lift sdlWindowSizeChangedEventsM
  sdlWindowMinimizedEventsM = lift sdlWindowMinimizedEventsM
  sdlWindowMaximizedEventsM = lift sdlWindowMaximizedEventsM
  sdlWindowRestoredEventsM = lift sdlWindowRestoredEventsM
  sdlWindowGainedMouseFocusEventsM = lift sdlWindowGainedMouseFocusEventsM
  sdlWindowLostMouseFocusEventsM = lift sdlWindowLostMouseFocusEventsM
  sdlWindowGainedKeyboardFocusEventsM = lift sdlWindowGainedKeyboardFocusEventsM
  sdlWindowLostKeyboardFocusEventsM = lift sdlWindowLostKeyboardFocusEventsM
  sdlWindowClosedEventsM = lift sdlWindowClosedEventsM
  sdlKeyboardEventsM = lift sdlKeyboardEventsM
  sdlTextEditingEventsM = lift sdlTextEditingEventsM
  sdlTextInputEventsM = lift sdlTextInputEventsM
  sdlMouseMotionEventsM = lift sdlMouseMotionEventsM
  sdlMouseButtonEventsM = lift sdlMouseButtonEventsM
  sdlMouseWheelEventsM = lift sdlMouseWheelEventsM
  sdlJoyAxisEventsM = lift sdlJoyAxisEventsM
  sdlJoyBallEventsM = lift sdlJoyBallEventsM
  sdlJoyHatEventsM = lift sdlJoyHatEventsM
  sdlJoyButtonEventsM = lift sdlJoyButtonEventsM
  sdlJoyDeviceEventsM = lift sdlJoyDeviceEventsM
  sdlControllerAxisEventsM = lift sdlControllerAxisEventsM
  sdlControllerButtonEventsM = lift sdlControllerButtonEventsM
  sdlControllerDeviceEventsM = lift sdlControllerDeviceEventsM
  sdlQuitEventM = lift sdlQuitEventM
  sdlUserEventsM = lift sdlUserEventsM
  sdlSysWMEventsM = lift sdlSysWMEventsM
  sdlTouchFingerEventsM = lift sdlTouchFingerEventsM
  sdlMultiGestureEventsM = lift sdlMultiGestureEventsM
  sdlDollarGestureEventsM = lift sdlDollarGestureEventsM
  sdlDropEventsM = lift sdlDropEventsM
  sdlClipboardUpdateEventsM = lift sdlClipboardUpdateEventsM