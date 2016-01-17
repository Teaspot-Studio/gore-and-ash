{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.GoreAndAsh.SDL.State(
    SDLState(..)
  , emptySDLState
  , flashSDLState
  ) where

import Control.DeepSeq 
import Data.Text 
import Data.Word
import GHC.Generics (Generic)
import Linear

import SDL.Event 
import SDL.Input.Keyboard
import SDL.Input.Mouse
import SDL.Internal.Types

import Data.Sequence (Seq)
import qualified Data.Sequence as S 

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H 

-- | Inner state of logger
data SDLState s = SDLState {
  sdlNextState :: !s
, sdlWindows :: !(HashMap Text (Window, Renderer, V4 Word8))

, sdlWindowShownEvents :: !(Seq WindowShownEventData)
, sdlWindowHiddenEvents :: !(Seq WindowHiddenEventData)
, sdlWindowExposedEvents :: !(Seq WindowExposedEventData)
, sdlWindowMovedEvents :: !(Seq WindowMovedEventData)
, sdlWindowResizedEvents :: !(Seq WindowResizedEventData)
, sdlWindowSizeChangedEvents :: !(Seq WindowSizeChangedEventData)
, sdlWindowMinimizedEvents :: !(Seq WindowMinimizedEventData)
, sdlWindowMaximizedEvents :: !(Seq WindowMaximizedEventData)
, sdlWindowRestoredEvents :: !(Seq WindowRestoredEventData)
, sdlWindowGainedMouseFocusEvents :: !(Seq WindowGainedMouseFocusEventData)
, sdlWindowLostMouseFocusEvents :: !(Seq WindowLostMouseFocusEventData)
, sdlWindowGainedKeyboardFocusEvents :: !(Seq WindowGainedKeyboardFocusEventData)
, sdlWindowLostKeyboardFocusEvents :: !(Seq WindowLostKeyboardFocusEventData)
, sdlWindowClosedEvents :: !(Seq WindowClosedEventData)
, sdlKeyboardEvents :: !(Seq KeyboardEventData)
, sdlTextEditingEvents :: !(Seq TextEditingEventData)
, sdlTextInputEvents :: !(Seq TextInputEventData)
, sdlMouseMotionEvents :: !(Seq MouseMotionEventData)
, sdlMouseButtonEvents :: !(Seq MouseButtonEventData)
, sdlMouseWheelEvents :: !(Seq MouseWheelEventData)
, sdlJoyAxisEvents :: !(Seq JoyAxisEventData)
, sdlJoyBallEvents :: !(Seq JoyBallEventData)
, sdlJoyHatEvents :: !(Seq JoyHatEventData)
, sdlJoyButtonEvents :: !(Seq JoyButtonEventData)
, sdlJoyDeviceEvents :: !(Seq JoyDeviceEventData)
, sdlControllerAxisEvents :: !(Seq ControllerAxisEventData)
, sdlControllerButtonEvents :: !(Seq ControllerButtonEventData)
, sdlControllerDeviceEvents :: !(Seq ControllerDeviceEventData)
, sdlQuitEvent :: !Bool
, sdlUserEvents :: !(Seq UserEventData)
, sdlSysWMEvents :: !(Seq SysWMEventData)
, sdlTouchFingerEvents :: !(Seq TouchFingerEventData)
, sdlMultiGestureEvents :: !(Seq MultiGestureEventData)
, sdlDollarGestureEvents :: !(Seq DollarGestureEventData)
, sdlDropEvents :: !(Seq DropEventData)
, sdlClipboardUpdateEvents :: !(Seq ClipboardUpdateEventData)
} deriving (Generic)

instance NFData s => NFData (SDLState s)
instance NFData WindowShownEventData 
instance NFData WindowHiddenEventData
instance NFData WindowExposedEventData 
instance NFData WindowMovedEventData 
instance NFData WindowResizedEventData 
instance NFData WindowSizeChangedEventData 
instance NFData WindowMinimizedEventData 
instance NFData WindowMaximizedEventData 
instance NFData WindowRestoredEventData 
instance NFData WindowGainedMouseFocusEventData 
instance NFData WindowLostMouseFocusEventData 
instance NFData WindowGainedKeyboardFocusEventData 
instance NFData WindowLostKeyboardFocusEventData 
instance NFData WindowClosedEventData 
instance NFData KeyboardEventData 
instance NFData TextEditingEventData 
instance NFData TextInputEventData 
instance NFData MouseMotionEventData 
instance NFData MouseButtonEventData 
instance NFData MouseWheelEventData 
instance NFData JoyAxisEventData 
instance NFData JoyBallEventData 
instance NFData JoyHatEventData 
instance NFData JoyButtonEventData 
instance NFData JoyDeviceEventData 
instance NFData ControllerAxisEventData 
instance NFData ControllerButtonEventData 
instance NFData ControllerDeviceEventData
instance NFData TouchFingerEventData 
instance NFData MultiGestureEventData 
instance NFData DollarGestureEventData 
instance NFData ClipboardUpdateEventData
instance NFData Keysym
instance NFData MouseButton
instance NFData InputMotion
instance NFData Scancode
instance NFData MouseDevice 
instance NFData Keycode 
instance NFData KeyModifier

instance NFData Window where
  rnf = (`seq` ())

instance NFData Renderer where
  rnf = (`seq` ())

instance NFData SysWMEventData where
  rnf SysWMEventData{..} = sysWMEventMsg `seq` ()

instance NFData UserEventData where
  rnf UserEventData{..} = userEventWindow 
    `seq` userEventCode
    `seq` userEventData1
    `seq` userEventData2
    `seq` ()

instance NFData DropEventData where
  rnf DropEventData{..} = dropEventFile `seq` ()

-- | Creates empty module state
emptySDLState :: s -> SDLState s 
emptySDLState s = SDLState {
    sdlNextState = s
  , sdlWindows = H.empty 

  , sdlWindowShownEvents = S.empty
  , sdlWindowHiddenEvents = S.empty
  , sdlWindowExposedEvents = S.empty
  , sdlWindowMovedEvents = S.empty
  , sdlWindowResizedEvents = S.empty
  , sdlWindowSizeChangedEvents = S.empty
  , sdlWindowMinimizedEvents = S.empty
  , sdlWindowMaximizedEvents = S.empty
  , sdlWindowRestoredEvents = S.empty
  , sdlWindowGainedMouseFocusEvents = S.empty
  , sdlWindowLostMouseFocusEvents = S.empty
  , sdlWindowGainedKeyboardFocusEvents = S.empty
  , sdlWindowLostKeyboardFocusEvents = S.empty
  , sdlWindowClosedEvents = S.empty
  , sdlKeyboardEvents = S.empty
  , sdlTextEditingEvents = S.empty
  , sdlTextInputEvents = S.empty
  , sdlMouseMotionEvents = S.empty
  , sdlMouseButtonEvents = S.empty
  , sdlMouseWheelEvents = S.empty
  , sdlJoyAxisEvents = S.empty
  , sdlJoyBallEvents = S.empty
  , sdlJoyHatEvents = S.empty
  , sdlJoyButtonEvents = S.empty
  , sdlJoyDeviceEvents = S.empty
  , sdlControllerAxisEvents = S.empty
  , sdlControllerButtonEvents = S.empty
  , sdlControllerDeviceEvents = S.empty
  , sdlQuitEvent = False
  , sdlUserEvents = S.empty
  , sdlSysWMEvents = S.empty
  , sdlTouchFingerEvents = S.empty
  , sdlMultiGestureEvents = S.empty
  , sdlDollarGestureEvents = S.empty
  , sdlDropEvents = S.empty
  , sdlClipboardUpdateEvents = S.empty
  }

-- | After full cycle of simulation all events are dropped
flashSDLState :: SDLState s -> SDLState s 
flashSDLState s = s {
    sdlWindowShownEvents = S.empty
  , sdlWindowHiddenEvents = S.empty
  , sdlWindowExposedEvents = S.empty
  , sdlWindowMovedEvents = S.empty
  , sdlWindowResizedEvents = S.empty
  , sdlWindowSizeChangedEvents = S.empty
  , sdlWindowMinimizedEvents = S.empty
  , sdlWindowMaximizedEvents = S.empty
  , sdlWindowRestoredEvents = S.empty
  , sdlWindowGainedMouseFocusEvents = S.empty
  , sdlWindowLostMouseFocusEvents = S.empty
  , sdlWindowGainedKeyboardFocusEvents = S.empty
  , sdlWindowLostKeyboardFocusEvents = S.empty
  , sdlWindowClosedEvents = S.empty
  , sdlKeyboardEvents = S.empty
  , sdlTextEditingEvents = S.empty
  , sdlTextInputEvents = S.empty
  , sdlMouseMotionEvents = S.empty
  , sdlMouseButtonEvents = S.empty
  , sdlMouseWheelEvents = S.empty
  , sdlJoyAxisEvents = S.empty
  , sdlJoyBallEvents = S.empty
  , sdlJoyHatEvents = S.empty
  , sdlJoyButtonEvents = S.empty
  , sdlJoyDeviceEvents = S.empty
  , sdlControllerAxisEvents = S.empty
  , sdlControllerButtonEvents = S.empty
  , sdlControllerDeviceEvents = S.empty
  , sdlQuitEvent = False
  , sdlUserEvents = S.empty
  , sdlSysWMEvents = S.empty
  , sdlTouchFingerEvents = S.empty
  , sdlMultiGestureEvents = S.empty
  , sdlDollarGestureEvents = S.empty
  , sdlDropEvents = S.empty
  , sdlClipboardUpdateEvents = S.empty
  }