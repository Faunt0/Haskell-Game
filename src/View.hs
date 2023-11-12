-- | This module defines how to turn
--   the game state into a picture
module View where
import Data.Map
import Graphics.Gloss
import Model
import Controller
import GameMechanics
import System.Directory (getDirectoryContents)
import GHC.Base (undefined)
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Prelude

view :: Map String Picture -> GameState -> IO Picture  --check status, display the corresponding view
view p gstate
  | status gstate == StartScreen = startScreenPic p
  | status gstate == Game = pics p gstate
  | status gstate == Pause = pausePic p gstate
  | status gstate == GameOver = pics p gstate

startScreenPic :: Map String Picture -> IO Picture
startScreenPic picturemap = do
  sz <- getScreenSize
  let sz2 = (fromIntegral (fst sz) ::Float, fromIntegral (snd sz) ::Float)
  files <- getDirectoryContents "saveFiles/"
  let firstFile = head files
  let filePic = ([Translate (-(fst sz2)/4) (- (snd sz2)/2.2) (scale 0.5 0.5 (color green (text ("[1] - " ++ remJson firstFile)))) | length files > 2])
  let p = [Translate (-(fst sz2)/6) ((snd sz2)/2.8) (scale 0.5 0.5 (color green (text "START MENU"))),Translate (-(fst sz2)/5) ((snd sz2)/4) (scale 0.3 0.3 (color green (text "PRESS ENTER TO START"))), Translate (-(fst sz2)/3.8) (- (snd sz2)/2.8) (color green (text "SAVE FILES:"))]
  let titlePic = [picturemap ! "title"]
  return (Pictures (p ++ filePic ++ titlePic))
  where
    remJson :: String -> String
    remJson s = [s !! i | i <- [0..length s - 6]]

pausePic :: Map String Picture -> GameState -> IO Picture
pausePic p gstate= do
  let pausepic = [Translate (-200) (100) (color green (text "[P]AUSE")), Translate (-200) (-300) (scale 0.5 0.5 (color green (text "PRESS [R] TO RESET"))), Translate (-200) (-400) (scale 0.5 0.5 (color green (text "PRESS [S] TO RESET")))]
  let savedpic = if infoToShow gstate == ShowAString "Saving" then Translate (-500) 300 (color green (text "SAVED TO save1.JSON")) else Blank
  gstatepic <- pics p (gstate {infoToShow = ShowNothing})
  return (Pictures (savedpic : gstatepic : pausepic))

pics :: Map String Picture -> GameState -> IO Picture --using a gamestate and a map of all pictures, go through all entities and return the picture
pics picturemap gstate = do
  lives <- displayStats gstate picturemap
  return (Pictures (
        entityrendermoon (background gstate) picturemap ++
        entityrendermountain (background gstate) picturemap ++
        entityrendercloud (background gstate) picturemap++
        [Translate x y (picturemap ! "ship") -- Player
        , Translate 30 30 (viewPure gstate)] -- info to show -- score
        ++ entityPics bts picturemap -- player bullets
        ++ entityPics (enemies gstate) picturemap -- render enemies
        ++ entityPics (concat (Prelude.map bullets (enemies gstate))) picturemap++lives -- enemy bullets
      ))
  where
    (Pt x y, s) = hitbox (player gstate)
    bts = bullets (player gstate)
    playerBullets = bullets (player gstate)

entityPics :: [Entity]-> Map String Picture-> [Picture] -- For all entities, typecheck and display the corresponding picture
entityPics [] _= []
entityPics (entity:es) picturemap= Translate x y pic : entityPics es picturemap
    where
      s = snd (hitbox entity)
      (Pt x y) = fst (hitbox entity)
      pic = case entityType entity of
            -- enemies
            Swarm -> picturemap ! "swarm"
            Turret -> scale 0.3 0.3 (picturemap ! "turret")
            Worm -> scale 1.5 1.5 (picturemap ! "worm")
            Boss -> color red (Polygon [(0, 0), (s, 0), (s, s), (0, s)])
            Explosion -> explosionstate (health entity) picturemap

            -- bullets
            Pea -> scale 0.1 0.1 (picturemap ! "pea")
            Rocket -> scale 0.2 0.2 (picturemap ! "rocket")
            Laserbeam -> color cyan (Line [(0, 0), (s, 0)])

entityrendercloud :: [Entity] -> Map String Picture -> [Picture] --to simulate parralax we had to make sure the background entities were rendered in the right order
entityrendercloud [] _ = []
entityrendercloud (entity:xs) picturemap | entityType entity == Cloud= Translate x y (picturemap ! "cloud") : entityrendercloud xs picturemap | otherwise = entityrendercloud xs picturemap
            where
            (Pt x y) = fst (hitbox entity)

entityrendermountain :: [Entity] -> Map String Picture -> [Picture]
entityrendermountain [] _ = []
entityrendermountain (entity:xs) picturemap | entityType entity == Mountain= Translate x y (picturemap ! "mountain") : entityrendermountain xs picturemap | otherwise = entityrendermountain xs picturemap
            where
            (Pt x y) = fst (hitbox entity)

entityrendermoon :: [Entity] -> Map String Picture -> [Picture]
entityrendermoon [] _ = []
entityrendermoon (entity:xs) picturemap | entityType entity == Planet = Translate x y (picturemap ! "moon") : entityrendermoon xs picturemap | otherwise = entityrendermoon xs picturemap
            where
            (Pt x y) = fst (hitbox entity)



explosionstate :: Float -> Map String Picture -> Picture   --change frame of explosion based of explosions health
explosionstate health picturemap| health >= 21*(1/fromIntegral fps) = picturemap ! "frame1"
                                | health >= 18*(1/fromIntegral fps) = picturemap ! "frame2"
                                | health >= 15*(1/fromIntegral fps) = picturemap ! "frame3"
                                | health >= 12*(1/fromIntegral fps) = picturemap ! "frame4"
                                | health >= 9*(1/fromIntegral fps) = picturemap ! "frame5"
                                | health >= 6*(1/fromIntegral fps) = picturemap ! "frame6"
                                | otherwise = picturemap ! "frame7"


displayStats :: GameState-> Map String Picture -> IO [Picture] --display a little plane in the top right corresponding to lives left
displayStats gstate picturemap= do
               sz <- getScreenSize
               let sz2 = (fromIntegral (fst sz ) ::Float, fromIntegral (snd sz) ::Float)
               let margin = snd sz2 *0.05
               let szx = (-0.5) * fst sz2
               let szy = 0.5 * snd sz2
               let healthBar = translate (szx+ margin) (szy-margin*4) (color red (text (show (score gstate)))):[translate ( szx + (margin * i)) ( szy -  30) (picturemap ! "ship") | i <- [1..health (player gstate)]]
               if health (player gstate) > 0 then return healthBar else return [translate 0 0 (picturemap ! "failed")]



viewScore :: GameState -> Picture
viewScore gstate = color red (text (show (score gstate)))

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> blank
  ShowAChar   c -> color green (text [c])
  ShowAString s -> color green (text s)