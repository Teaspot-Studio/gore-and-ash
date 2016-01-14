module Game.GoreAndAsh.SDL.API(
    MonadSDL(..)
  , WindowConfig(..)
  , RendererConfig(..)
  , RendererType(..)
  , Window
  , Renderer
  , module ReExport
  -- | Keyboard arrow API
  , keyScancode
  , keyPress
  , keyRelease
  -- | Mouse arrow API
  , mouseScroll
  , mouseScrollX
  , mouseScrollY
  ) where

import Control.Lens ((^.))
import Control.Monad.Catch
import Control.Monad.State.Strict
import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Int 
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics 
import Linear 
import Prelude hiding (id, (.))
import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S 

import SDL as ReExport hiding (get, Event)
import SDL.Event as ReExport hiding (Event)
import SDL.Input.Keyboard.Codes as ReExport

import Game.GoreAndAsh
import Game.GoreAndAsh.SDL.State
import Game.GoreAndAsh.SDL.Module

-- | Module specific exceptions
data SDL'ModuleException = 
  -- | Tried to register two windows with equal names
  SDL'ConflictingWindows !Text
  deriving (Generic, Show)

instance Exception SDL'ModuleException

-- | Low level API for module
class (MonadIO m, MonadThrow m) => MonadSDL m where 
  -- | Creates window and stores in module context
  --
  -- Throws @SDL'ConflictingWindows@ on name conflict
  sdlCreateWindowM :: 
       Text -- ^ Window name that is used to get the window (and renderer) from the module later
    -> Text -- ^ Title of the window
    -> WindowConfig -- ^ Window configuration
    -> RendererConfig -- ^ Renderer configuration
    -> m (Window, Renderer)

  -- | Getting window and renderer by name
  sdlGetWindowM :: 
       Text -- ^ Window name that was used at @sdlCreateWindowM@ call
    -> m (Maybe (Window, Renderer))

  -- | Destroying window and renderer by name
  sdlDestroyWindowM ::
       Text -- ^ Window name that was used at @sdlCreateWindowM@ call
    -> m ()

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

instance {-# OVERLAPPING #-} (MonadIO m, MonadThrow m) => MonadSDL (SDLT s m) where
  sdlCreateWindowM n t wc rc = do 
    w <- createWindow t wc
    r <- createRenderer w (-1) rc
    s <- SDLT get
    case H.lookup n . sdlWindows $! s of 
      Just _ -> throwM . SDL'ConflictingWindows $! n
      Nothing -> do
        SDLT . put $! s {
            sdlWindows = H.insert n (w, r) . sdlWindows $! s
          }
        return (w, r)

  sdlGetWindowM n = do 
    s <- SDLT get 
    return . H.lookup n . sdlWindows $! s 

  sdlDestroyWindowM n = do 
    s <- SDLT get 
    case H.lookup n . sdlWindows $! s of 
      Just (w, r) -> do 
        destroyRenderer r 
        destroyWindow w
        SDLT . put $! s {
          sdlWindows = H.delete n . sdlWindows $! s
        }
      Nothing -> return ()

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

instance {-# OVERLAPPABLE #-} (MonadIO (mt m), MonadThrow (mt m), MonadSDL m, MonadTrans mt) => MonadSDL (mt m) where 
  sdlCreateWindowM n t wc rc = lift $ sdlCreateWindowM n t wc rc
  sdlGetWindowM = lift . sdlGetWindowM
  sdlDestroyWindowM = lift . sdlDestroyWindowM

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

-- | Fires when specific scancode key is pressed/unpressed
keyScancode :: MonadSDL m => Scancode -> InputMotion -> GameWire m a (Event (Seq KeyboardEventData))
keyScancode sc im = liftGameMonad $ do 
  es <- sdlKeyboardEventsM
  return $! if S.null es 
    then NoEvent
    else Event . S.filter isNeeded $! es 
  where
    isNeeded KeyboardEventData{..} = case keysymScancode keyboardEventKeysym of 
      sc -> keyboardEventKeyMotion == im
      _ -> False

-- | Fires when specific scancode key is pressed
keyPress :: MonadSDL m => Scancode -> GameWire m a (Event (Seq KeyboardEventData))
keyPress sc = keyScancode sc Pressed 

-- | Fires when specific scancode key is released
keyRelease :: MonadSDL m => Scancode -> GameWire m a (Event (Seq KeyboardEventData))
keyRelease sc = keyScancode sc Released 

-- | Returns accumulated mouse scroll scince last frame
mouseScroll :: MonadSDL m => GameWire m a (Event (V2 Int32))
mouseScroll = liftGameMonad $ do 
  es <- sdlMouseWheelEventsM
  return $! if S.null es 
    then NoEvent
    else Event . sumV . fmap mouseWheelEventPos $! es

-- | Returns accumulated mouse scroll scince last frame
mouseScrollX :: MonadSDL m => GameWire m a (Event Int32)
mouseScrollX = mapE (^. _x) . mouseScroll

-- | Returns accumulated mouse scroll scince last frame
mouseScrollY :: MonadSDL m => GameWire m a (Event Int32)
mouseScrollY = mapE (^. _y) . mouseScroll