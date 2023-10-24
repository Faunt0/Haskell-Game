-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . circ


circ :: GameState -> Picture
circ gstate = 
  Pictures ([
      Translate x y (color green (Circle 10)) -- Player
    , Translate 30 30 (viewPure gstate)
    , Translate 50 50 (viewScore gstate)
    ] ++ [Translate bx by (color blue (Circle 5)) | (Pt bx by) <- b] ++ 
     [Translate xe ye (color red (Polygon [(0, 0), (s, 0), (s, s), (0, 10)])) | Swarm h s (Pt xe ye) <- enemies gstate]) -- enemy)
  where
    (P (Pt x y) _ _ _ b) = player gstate


viewScore :: GameState -> Picture
viewScore gstate = color green (text (show (score gstate)))

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])