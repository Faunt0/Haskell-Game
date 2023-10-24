module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = Graphics.Gloss.Interface.IO.Game.playIO (Graphics.Gloss.Interface.IO.Game.InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
              Graphics.Gloss.Interface.IO.Game.black            -- Background color
              10               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function