module Main where
import Data.Map
import Controller
import Model
import View
import GameMechanics
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
-- main = Graphics.Gloss.Interface.IO.Game.playIO (Graphics.Gloss.Interface.IO.Game.InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
main = do  
    s <- loadBMP "tiny_ship.bmp" 
    pea <- loadBMP "pea.bmp"
    swarm <- loadBMP "swarm.bmp"
    worm <- loadBMP "worm.bmp"
    splosions <- mapM loadBMP ["splosieframes/splosieframe1.bmp","splosieframes/splosieframe2.bmp","splosieframes/splosieframe3.bmp",
                               "splosieframes/splosieframe4.bmp","splosieframes/splosieframe5.bmp","splosieframes/splosieframe6.bmp",
                               "splosieframes/splosieframe7.bmp"]
    let splosions2 = zip ["frame1","frame2","frame3","frame4","frame5","frame6","frame7"] splosions 
    --splosieframes <- mapM loadBMP ["splosieframes/splosieframe1.bmp","splosieframes/splosieframe2.bmp"]
    Graphics.Gloss.Interface.IO.Game.playIO Graphics.Gloss.Interface.IO.Game.FullScreen
              Graphics.Gloss.Interface.IO.Game.black            -- Background color
              fps               -- Frames per second
              initialState     -- Initial state
              (view (Data.Map.fromList ([("ship",s),("pea",pea),("swarm", swarm),("worm", worm)]++splosions2)))          -- View function
              input            -- Event function
              step             -- Step function
              