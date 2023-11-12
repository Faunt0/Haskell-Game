
module Offscreen where

import Model
import GameMechanics

-- may want to add a margin so the enemy does not immediately despawn the second it hits the edge of the screen
entityOffscreen :: [Entity] -> (Int, Int) -> [Entity]
entityOffscreen [] _ = []
entityOffscreen (e:bts) (x, y)
    | isbullet && (offscreen_Xaxis || offscreen_Yaxis) = entityOffscreen bts (x, y)
    | offscreen_Xaxis || offscreen_Yaxis = entityOffscreen bts (x, y) -- only checks on the x not the y and only the right part of the screen not the left (is this necessary? think boomerangs)
    | otherwise = e : entityOffscreen bts (x, y)
    where
      (Pt ex ey) = fst (hitbox e)
      margin = 0
      isbullet = elem (entityType e) [Pea, Rocket, Laserbeam]
      offscreen_Xaxis = ex >= fromIntegral (x `div` 2) + xmargin + 100 || ex <= - fromIntegral (x `div` 2) - xmargin - 100
      offscreen_Yaxis = False --ey >= fromIntegral (y `div` 2) - margin || ey <= - fromIntegral (y `div` 2) + margin