-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import qualified Controller

view :: GameState -> IO Picture
view = return . circ


circ :: GameState -> Picture
circ gstate = 
  Pictures ([
      Translate x y (color green (Circle 10)) -- Player
    , Translate 30 30 (viewPure gstate) -- info to show
    , Translate 50 50 (viewScore gstate) -- 
    ] ++ [Translate bx by (color blue (Circle 4)) | (Pt bx by) <- map Controller.bulletPos bts] 
    ++ enemiesPics (enemies gstate)
    ) -- enemy)
  where
    (P (Pt x y) _ _ _ bts) = player gstate


enemiesPics :: [Enemy] -> [Picture]
enemiesPics [] = []
enemiesPics (enemy:es) = enemiesPics es ++ case enemy of
                            Swarm _ (Pt x y) s -> [Translate x y (color red (Polygon [(0, 0), (s, 0), (s, s), (0, s)]))]
                            Turret _ (Pt x y) s -> [Translate x y (color blue (Polygon [(0, 0), (s, 0), (s, s), (0, s)]))]
                            Worm _ (Pt x y) s -> [Translate x y (color yellow (Polygon [(0, 0), (s, 0), (s, s), (0, s)]))]
                            Boss _ (Pt x y) s -> [Translate x y (color red (Polygon [(0, 0), (s, 0), (s, s), (0, s)]))]



viewScore :: GameState -> Picture
viewScore gstate = color green (text (show (score gstate)))

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])
  ShowAString s -> color green (text s)