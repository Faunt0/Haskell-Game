-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import qualified Controller

view :: GameState -> IO Picture
view = return . pics


pics :: GameState -> Picture
pics gstate = 
  Pictures ([
      Translate x y (color green (Circle s)) -- Player
    , Translate 30 30 (viewPure gstate) -- info to show
    , Translate 50 50 (viewScore gstate)] 
    ++ bulletPics bts -- player bullets
    ++ enemiesPics (enemies gstate)
    ++ bulletPics (enemyBullets gstate) -- enemybullets
    )
  where
    (P ((Pt x y), s) _ _ _ _ bts) = player gstate
    playerBullets = bullets (player gstate)

-- enemyBulletPics :: [Enemy] -> [Picture]
-- enemyBulletPics [] = []
-- enemyBulletPics (e:es) = enemyBulletPics es ++ bulletPics (enemyBullets e)

bulletPics :: [Bullet] -> [Picture]
bulletPics [] = []
bulletPics (b:bts) = bulletPic : bulletPics bts
    where 
      s = snd (bulletHitbox b)
      (Pt x y) = fst (bulletHitbox b)
      bulletPic = case bulletType b of
            Pea -> Translate x y (color blue (Circle s))
            Rocket -> Translate x y (color yellow (Circle s))
            Laserbeam -> Translate x y (color cyan (Line [(0, 0), (s, 0)]))


enemiesPics :: [Enemy] -> [Picture]
enemiesPics [] = []
enemiesPics (enemy:es) = enemyPic : enemiesPics es
    where 
      s = snd (enemyHitBox enemy)
      (Pt x y) = fst (enemyHitBox enemy)
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