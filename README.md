# Haskell-Game
This is a variety on the game Shoot-em-Up where you have to kill swarms of aliens attacking you from the right side of the screen.

## Get It To Run
To get our game to run, please follow the following steps:
1. Use the command `cabal build` in the directory directly containing the `.cabal` file.
3. If there is not already a folder called "saveFiles" in the same directory as in step 1, make it, there does not need to be anything in it.
2. Use the command `cabal run` in the same directory as in step 1.

# Controls
## Start menu:
* "ENTER" - Press "enter" at the start menu to start the game from its initial state
* "1" - Press "1" to load a save file if there is one

## Game
* "w" - Press "w" to move up on the screen
* "a" - Press "a" to move left on the screen
* "s" - Press "s" to move down on the screen
* "d" - Press "d" to move right on the screen
* "SPACE" - Press the "SPACE" bar to shoot bullets, this happens at a certain rate depending on your weapon
* "TAB" - Press the "TAB" key to switch weapons
* "p" - Press "p" to put the game on pause.

## Pause
* "p" - Press "p" during a pause to resume playing the game
* "s" - Press "s" during a pause to save the game to the one save slot
* "r" - Press "r" during a pause to reset the game to the start menu

## Game Over:
* "r" - Press "r" to reset the game to the start screen

# Enemies
## there are 3 kinds of enemies:
* Swarm - this is a smaller enemy which spawns more often and shoots bullets at the player
* Brute - this is a larger enemy which shoots 4 bullets along each diagonal and explodes when it dies
* Turret - this is larger enemy which moves along the bottom of the screen and shoots bullets at the player

# Guns
## There are 3 guns the player can choose from
* Peashooter - This gun has a medium rate of fire and does a bit of damage
* Launcher - This gun has a slow rate of fire and shoots exploding rockets
* Lasergun - This gun is broken on all accounts