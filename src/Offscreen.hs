
module Offscreen where

import Model
import GameMechanics

-- | Remove for a list of entities those that are out of our bounds
entityOffscreen :: [Entity] -> (Int, Int) -> [Entity]
entityOffscreen [] _ = []
entityOffscreen (e:bts) (x, y)
    | offscreen_Xaxis = entityOffscreen bts (x, y)
    | otherwise = e : entityOffscreen bts (x, y)
    where
      (Pt ex ey) = fst (hitbox e)
      margin = 0
      offscreen_Xaxis = ex >= fromIntegral (x `div` 2) + xmargin + 100 || ex <= - fromIntegral (x `div` 2) - xmargin - 100