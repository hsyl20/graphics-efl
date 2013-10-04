import Graphics.Efl.Simple

main :: IO ()
main = do
   withDefaultWindow $ \ _ canvas -> do

      r <- addRectangle canvas
      resize r 100 40
      move r 20 40
      setColor r 255 0 0 255
      uncover r

      t <- addText canvas
      setText t "Haskell-EFL!!"
      resize t 200 100
      move t 25 50
      setTextStyle t EvasTextStylePlain EvasTextStyleShadowDirectionBottomRight
      setTextFont t "DejaVu" 14
      setColor t 0 255 0 255
      uncover t

      renderCanvas canvas
