
-- module Offscreen where

-- import Model
-- import Controller

-- -- may want to add a margin so the enemy does not immediately despawn the second it hits the edge of the screen
-- bulletOffscreen :: [Bullet] -> (Int, Int) -> [Bullet]
-- bulletOffscreen [] _ = []
-- bulletOffscreen (b:bts) (x, y)
--     | fromIntegral (x `div` 2) - 100 <= bx = bulletOffscreen bts (x, y)
--     | otherwise = b : bulletOffscreen bts (x, y)
--     where
--       (Pt bx by) = bulletPos b
-- enemyOffscreen :: [Enemy] -> (Int, Int) -> [Enemy]
-- enemyOffscreen [] _ = []
-- enemyOffscreen (b:bts) (x, y)
--     | fromIntegral (x `div` 2) <= ex = enemyOffscreen bts (x, y)
--     | otherwise = b : enemyOffscreen bts (x, y)
--     where
--       (_, Pt ex ey, _) = enemyInf b