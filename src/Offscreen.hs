
module Offscreen where

import Model

-- may want to add a margin so the enemy does not immediately despawn the second it hits the edge of the screen
bulletOffscreen :: [Bullet] -> (Int, Int) -> [Bullet]
bulletOffscreen [] _ = []
bulletOffscreen (b:bts) (x, y)
    | fromIntegral (x `div` 2) - margin <= bx = bulletOffscreen bts (x, y) -- only checks on the x not the y and only the right part of the screen not the left (is this necessary? think boomerangs)
    | otherwise = b : bulletOffscreen bts (x, y)
    where
      (Pt bx by) = bulletPosition b
      margin = 100
enemyOffscreen :: [Enemy] -> (Int, Int) -> [Enemy]
enemyOffscreen [] _ = []
enemyOffscreen (e:es) (x, y)
    | fromIntegral (x `div` 2) <= ex || ex < - fromIntegral (x `div` 2) = enemyOffscreen es (x, y)
    | otherwise = e : enemyOffscreen es (x, y)
    where
      Pt ex ey = enemyPosition e
    --   margin = 100