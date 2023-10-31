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
    , Translate 50 50 (viewScore gstate)] 
    ++ bulletPics bts
    ++ enemiesPics (enemies gstate)
    ) -- enemy)
  where
    (P (Pt x y) _ _ _ _ bts) = player gstate
    playerBullets = bullets (player gstate)



-- another reason wy introducing entities might be a good idea
bulletPics :: [Bullet] -> [Picture]
bulletPics [] = []
bulletPics (b:bts) = bulletPic : bulletPics bts
    where 
      s = bulletSize b
      (Pt x y) = bulletPosition b
      bulletPic = case bulletType b of
            Pea -> Translate x y (color blue (Circle 5))
            Rocket -> Translate x y (color yellow (Circle 10))
            Laserbeam -> Translate x y (color cyan (Line [(0, 0), (s, 0)]))


enemiesPics :: [Enemy] -> [Picture]
enemiesPics [] = []
enemiesPics (enemy:es) = enemyPic : enemiesPics es
    where 
      s = enemySize enemy
      (Pt x y) = enemyPosition enemy
      enemyPic = case enemySpecies enemy of
            Swarm -> Translate x y (color red (Polygon [(0, 0), (s, 0), (s, s), (0, s)]))
            Turret -> Translate x y (color blue (Polygon [(0, 0), (s, 0), (s, s), (0, s)]))
            Worm -> Translate x y (color yellow (Polygon [(0, 0), (s, 0), (s, s), (0, s)]))
            Boss -> Translate x y (color red (Polygon [(0, 0), (s, 0), (s, s), (0, s)]))
            



viewScore :: GameState -> Picture
viewScore gstate = color red (text (show (score gstate)))

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])
  ShowAString s -> color green (text s)

-- background elements