module Main where
import Data.Map
import Controller
import Model
import View
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
-- main = Graphics.Gloss.Interface.IO.Game.playIO (Graphics.Gloss.Interface.IO.Game.InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
main = do  
    s <- loadBMP "tiny_ship.bmp" 
    pea <- loadBMP "pea.bmp"
    
    --splosieframes <- mapM loadBMP ["splosieframes/splosieframe1.bmp","splosieframes/splosieframe2.bmp"]
    Graphics.Gloss.Interface.IO.Game.playIO Graphics.Gloss.Interface.IO.Game.FullScreen
              Graphics.Gloss.Interface.IO.Game.black            -- Background color
              60               -- Frames per second
              initialState     -- Initial state
              (view (Data.Map.fromList [("ship",s),("pea",pea)]))          -- View function
              input            -- Event function
              step             -- Step function
              