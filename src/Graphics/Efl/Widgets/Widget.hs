module Graphics.Efl.Widgets.Widget where

class BasicObject o where
   peerObject :: Object
   clippingObject :: o -> Property (Maybe 
