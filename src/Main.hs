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
    bmps <- bitmaploadin
    Graphics.Gloss.Interface.IO.Game.playIO Graphics.Gloss.Interface.IO.Game.FullScreen
              Graphics.Gloss.Interface.IO.Game.black            -- Background color
              fps               -- Frames per second
              initialState     -- Initial state
              (view bmps)    -- View function, turn the list of [(String, Picture)] into a Map
              input            -- Event function
              step             -- Step function


bitmaploadin :: IO (Map String Picture)   --load in all bitmaps from bitmap folder, zip them with name and turn it into a map for easyu use in view
bitmaploadin = do  
    s <- loadBMP "bitmaps/tiny_ship.bmp" 
    failed <- loadBMP "bitmaps/Mission failed.bmp"
    pea <- loadBMP "bitmaps/pea.bmp"
    swarm <- loadBMP "bitmaps/swarm.bmp"
    worm <- loadBMP "bitmaps/worm.bmp"
    cloud <- loadBMP "bitmaps/Cloud.bmp"
    moon <- loadBMP "bitmaps/Moon.bmp"
    mountain <- loadBMP "bitmaps/Mountain.bmp"
    splosions <- mapM loadBMP ["bitmaps/splosieframes/splosieframe1.bmp",
                               "bitmaps/splosieframes/splosieframe2.bmp",
                               "bitmaps/splosieframes/splosieframe3.bmp",
                               "bitmaps/splosieframes/splosieframe4.bmp",
                               "bitmaps/splosieframes/splosieframe5.bmp",
                               "bitmaps/splosieframes/splosieframe6.bmp",
                               "bitmaps/splosieframes/splosieframe7.bmp"]
    let splosions2 = zip ["frame1","frame2","frame3","frame4","frame5","frame6","frame7"] splosions 
    return (Data.Map.fromList ([("ship",s),("pea",pea),("swarm", swarm),("worm", worm),("failed",failed),("cloud",cloud),("moon",moon),("mountain",mountain)]++splosions2))
              