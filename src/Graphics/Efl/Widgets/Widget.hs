module Graphics.Efl.Widgets.Widget where

import Graphics.Efl.Widgets.Reactive

class Widget w where
   widgetSize :: w -> Property (Int,Int)
   widgetPosition :: w -> Property (Int,Int)
   widgetVisible :: w -> Property Bool
