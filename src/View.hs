-- | This module defines how to turn
--   the game state into a picture
module View where
import Data.Map 
import Graphics.Gloss
import Model
import Controller
import GameMechanics
import System.Directory (getDirectoryContents)

view :: Map String Picture -> GameState -> IO Picture
view p gstate
  | status gstate == StartScreen = startScreenPic
  | status gstate == Game = return (pics p gstate)
  | status gstate == Pause = return (pics p gstate)
  | status gstate == GameOver = return (pics p gstate)
  -- does not need an otherwise since we've exhausted all cases of status

startScreenPic :: IO Picture
startScreenPic = do 
  files <- getDirectoryContents "saveFiles/"
  let firstFile = head files
  let filePic = [Translate (-200) (-140) (scale 0.5 0.5 (color green (text firstFile)))]
  let p = [Translate (-200) (200) (color green (text "START MENU")), Translate (-200) (0) (color green (text "SAVE FILES:"))]
  return (Pictures (p ++ filePic))

pics :: Map String Picture -> GameState -> Picture
pics picturemap gstate = 
  Pictures ([
    Translate x y (picturemap ! "ship") -- Player
      --Translate x y (color green (Circle s)) -- Player
    , Translate 30 30 (viewPure gstate) -- info to show
    , Translate 0 400 (scale 0.5 0.5 (viewScore gstate))] -- score
    ++ entityPics bts picturemap -- player bullets
    ++ entityPics (enemies gstate) picturemap -- render enemies
    ++ entityPics (flatten (Prelude.map bullets (enemies gstate))) picturemap -- enemy bullets
    )
  where
    (Pt x y, s) = hitbox (player gstate)
    bts = bullets (player gstate)
    playerBullets = bullets (player gstate)





entityPics :: [Entity] -> Map String Picture -> [Picture]
entityPics [] _= []
entityPics (entity:es) picturemap= Translate x y pic : entityPics es picturemap
    where 
      s = snd (hitbox entity)
      (Pt x y) = fst (hitbox entity)
      pic = case entityType entity of
            -- enemies
            Swarm -> picturemap ! "swarm"
            Turret -> color blue (Polygon [(0, 0), (s, 0), (s, s), (0, s)])
            Worm -> scale 1.5 1.5 (picturemap ! "worm")
            Boss -> color red (Polygon [(0, 0), (s, 0), (s, s), (0, s)])
            Explosion -> explosionstate (health entity) picturemap
                      
            -- bullets
            Pea -> scale 0.1 0.1 (picturemap ! "pea")
            Rocket -> color yellow (Circle s)
            Laserbeam -> color cyan (Line [(0, 0), (s, 0)])
      

explosionstate :: Float -> Map String Picture -> Picture
explosionstate health picturemap| health >= 21*(1/fromIntegral fps) = picturemap ! "frame1"
                                | health >= 18*(1/fromIntegral fps) = picturemap ! "frame2"
                                | health >= 15*(1/fromIntegral fps) = picturemap ! "frame3"
                                | health >= 12*(1/fromIntegral fps) = picturemap ! "frame4"
                                | health >= 9*(1/fromIntegral fps) = picturemap ! "frame5"
                                | health >= 6*(1/fromIntegral fps) = picturemap ! "frame6"
                                | otherwise = picturemap ! "frame7"

viewScore :: GameState -> Picture
viewScore gstate = color red (text (show (score gstate)))

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])
  ShowAString s -> color green (text s)

-- background elements