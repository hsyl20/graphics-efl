module Graphics.Efl.Widgets.Layout where

import Graphics.Efl.Widgets.Reactive
import Graphics.Efl.Widgets.Widget

alignCenter :: (Widget a, Widget b) => a -> b -> IO ()
alignCenter a b = widgetPosition a =& do
   (bx,by) <- readProperty (widgetPosition b)
   (bw,bh) <- readProperty (widgetSize b)
   (w,h) <- readProperty (widgetSize a)
   return (bx + (bw-w) `div` 2, by + (bh-h) `div` 2)
