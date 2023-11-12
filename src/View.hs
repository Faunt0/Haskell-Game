-- | This module defines how to turn
--   the game state into a picture
module View where
import Data.Map 
import Graphics.Gloss
import Model
import Controller
import GameMechanics
import GHC.Base (undefined)
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Prelude 

view :: Map String Picture -> GameState -> IO Picture
view p gstate= pics p gstate
--test

pics :: Map String Picture -> GameState -> IO Picture
pics picturemap gstate
  | status gstate == StartScreen = return (Translate (-200) (-50) (color green (text "Start!")))
  -- | status gstate == GameOver = Translate (-200) (-50) (color green (text "Start!"))
  | otherwise =do 
               lives <- displayLives (player gstate) picturemap
               return (Pictures ([
                Translate x y (picturemap ! "ship") -- Player
      --Translate x y (color green (Circle s)) -- Player
                , Translate 30 30 (viewPure gstate) -- info to show
                , Translate 0 400 (scale 0.5 0.5 (viewScore gstate))] -- score
                ++ entityPics bts picturemap -- player bullets
                ++ entityPics (enemies gstate) picturemap -- render enemies
                ++ entityPics (flatten (Prelude.map bullets (enemies gstate))) picturemap++lives -- enemy bullets
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
            Turret -> color blue (Polygon [(0, 0), (s, 0), (s, s), (0, s)])
            Worm -> scale 1.5 1.5 (picturemap ! "worm")
            Boss -> color red (Polygon [(0, 0), (s, 0), (s, s), (0, s)])
            Explosion -> explosionstate (health entity) picturemap
                      
            -- bullets
            Pea -> scale 0.1 0.1 (picturemap ! "pea")
            Rocket -> color yellow (Circle s)
            Laserbeam -> color cyan (Line [(0, 0), (s, 0)])
      

explosionstate :: Float -> Map String Picture -> Picture  --Display a frame of the explosion animation based of the health of the explosion entity
explosionstate health picturemap| health >= 21*(1/fromIntegral fps) = picturemap ! "frame1"
                                | health >= 18*(1/fromIntegral fps) = picturemap ! "frame2"
                                | health >= 15*(1/fromIntegral fps) = picturemap ! "frame3"
                                | health >= 12*(1/fromIntegral fps) = picturemap ! "frame4"
                                | health >= 9*(1/fromIntegral fps) = picturemap ! "frame5"
                                | health >= 6*(1/fromIntegral fps) = picturemap ! "frame6"
                                | otherwise = picturemap ! "frame7"

viewScore :: GameState -> Picture
viewScore gstate = color red (text (show (score gstate)))


displayLives :: Player-> Map String Picture -> IO [Picture]
displayLives playa picturemap= do 
               sz <- getScreenSize 
               let sz2 = (fromIntegral (fst sz ) ::Float, fromIntegral (snd sz) ::Float)
               let margin = snd sz2 *0.05 
               let szx = (-0.5) * fst sz2
               let szy = 0.5 * snd sz2
               case health playa of 
                5 -> return  [translate ( szx + margin) ( szy -  30) (picturemap ! "ship"), 
                             translate ( szx + (margin * 2)) ( szy -  30) (picturemap ! "ship"),
                             translate ( szx + (margin * 3)) ( szy -  30) (picturemap ! "ship"),
                             translate ( szx + (margin * 4)) ( szy -  30) (picturemap ! "ship"),
                             translate ( szx + (margin * 5)) ( szy -  30) (picturemap ! "ship")]
                4 -> return  [translate ( szx + margin) ( szy -  30) (picturemap ! "ship"), 
                             translate ( szx + (margin * 2)) ( szy -  30) (picturemap ! "ship"),
                             translate ( szx + (margin * 3)) ( szy -  30) (picturemap ! "ship"),
                             translate ( szx + (margin * 4)) ( szy -  30) (picturemap ! "ship")]
                3 -> return  [translate ( szx + margin) ( szy -  30) (picturemap ! "ship"), 
                             translate ( szx + (margin * 2)) ( szy -  30) (picturemap ! "ship"),
                             translate ( szx + (margin * 3)) ( szy -  30) (picturemap ! "ship")]
                2 -> return  [translate ( szx + margin) ( szy -  30) (picturemap ! "ship"), 
                             translate ( szx + (margin * 2)) ( szy -  30) (picturemap ! "ship")]
                1 -> return  [translate ( szx + margin) ( szy -  30) (picturemap ! "ship")]
                0 -> return  [translate 0 0 (picturemap ! "failed")]

             

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])
  ShowAString s -> color green (text s)

-- background elements