-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . circ

squares :: GameState -> Picture
squares g = color green (Polygon [(0, 0), (10, 0), (10, 10), (0, 10)])

circ :: GameState -> Picture
circ gstate = color green (Circle 5)

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])