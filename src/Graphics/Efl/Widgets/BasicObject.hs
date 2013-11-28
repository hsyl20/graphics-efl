module Graphics.Efl.Widgets.BasicObject where

import Graphics.Efl.Widgets.Reactive
import qualified Graphics.Efl.Canvas as Peer

class BasicObject o where
   peerObject :: o -> Peer.Object
   onMouseDown :: o -> Signal Peer.MouseDownEvent
   onMouseUp :: o -> Signal Peer.MouseUpEvent
   onMouseIn :: o -> Signal Peer.MouseInEvent
   onMouseOut :: o -> Signal Peer.MouseOutEvent
   onMouseWheel :: o -> Signal Peer.MouseWheelEvent
   onMouseMove :: o -> Signal Peer.MouseMoveEvent
   onMultiDown :: o -> Signal Peer.MultiDownEvent
   onMultiUp :: o -> Signal Peer.MultiUpEvent
   onMultiMove :: o -> Signal Peer.MultiMoveEvent
   onFree :: o -> Signal ()
   onKeyDown :: o -> Signal Peer.KeyDownEvent
   onKeyUp :: o -> Signal Peer.KeyUpEvent
   isFocused :: o -> Property Bool


initBasicObject :: BasicObject o => o -> IO ()
initBasicObject o = do
   Peer.onMouseDown (\_ info -> triggerSignal (onMouseDown o) info) (peerObject o)
   Peer.onMouseUp (\_ info -> triggerSignal (onMouseUp o) info) (peerObject o)
   Peer.onMouseIn (\_ info -> triggerSignal (onMouseIn o) info) (peerObject o)
   Peer.onMouseOut (\_ info -> triggerSignal (onMouseOut o) info) (peerObject o)
   Peer.onMouseWheel (\_ info -> triggerSignal (onMouseWheel o) info) (peerObject o)
   Peer.onMouseMove (\_ info -> triggerSignal (onMouseMove o) info) (peerObject o)
   Peer.onMultiUp (\_ info -> triggerSignal (onMultiUp o) info) (peerObject o)
   Peer.onMultiDown (\_ info -> triggerSignal (onMultiDown o) info) (peerObject o)
   Peer.onMultiMove (\_ info -> triggerSignal (onMultiMove o) info) (peerObject o)
   Peer.onFree (triggerSignal (onFree o) ()) (peerObject o)
   Peer.onKeyUp (\_ info -> triggerSignal (onKeyUp o) info) (peerObject o)
   Peer.onKeyDown (\_ info -> triggerSignal (onKeyDown o) info) (peerObject o)

   Peer.onObjectFocusIn (triggerPropertyEvent (isFocused o)) (peerObject o)
   Peer.onObjectFocusOut (triggerPropertyEvent (isFocused o)) (peerObject o)
