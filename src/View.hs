-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Controller

view :: GameState -> IO Picture
view = return . pics


pics :: GameState -> Picture
pics gstate
  | status gstate == StartScreen = Translate (-200) (-50) (color green (text "Start!"))
  -- | status gstate == GameOver = Translate (-200) (-50) (color green (text "Start!"))
  | otherwise =
  Pictures ([
      Translate x y (color green (Circle s)) -- Player
    , Translate 30 30 (viewPure gstate) -- info to show
    , Translate 0 400 (scale 0.5 0.5 (viewScore gstate))] -- score
    ++ entityPics bts -- player bullets
    ++ entityPics (enemies gstate) -- render enemies
    ++ entityPics (flatten (map bullets (enemies gstate))) -- enemy bullets
    )
  where
    ((Pt x y), s) = hitbox (player gstate)
    bts = bullets (player gstate)
    playerBullets = bullets (player gstate)



entityPics :: [Entity] -> [Picture]
entityPics [] = []
entityPics (entity:es) = pic : entityPics es
    where 
      s = snd (hitbox entity)
      (Pt x y) = fst (hitbox entity)
      pic = case entityType entity of
            -- enemies
            Swarm -> Translate x y (color red (Polygon [(0, 0), (s, 0), (s, s), (0, s)]))
            Turret -> Translate x y (color blue (Polygon [(0, 0), (s, 0), (s, s), (0, s)]))
            Worm -> Translate x y (color yellow (Polygon [(0, 0), (s, 0), (s, s), (0, s)]))
            Boss -> Translate x y (color red (Polygon [(0, 0), (s, 0), (s, s), (0, s)]))
            -- bullets
            Pea -> Translate x y (color blue (Circle s))
            Rocket -> Translate x y (color yellow (Circle s))
            Laserbeam -> Translate x y (color cyan (Line [(0, 0), (s, 0)]))
            -- _ -> Blank -- is this necessary?

            
viewScore :: GameState -> Picture
viewScore gstate = color red (text (show (score gstate)))

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])
  ShowAString s -> color green (text s)

-- background elements