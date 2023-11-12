module GameMechanics where

fps :: Int
fps = 60 


xmargin :: Float
ymargin :: Float
xmargin = 400
ymargin = 200


swarmRoF :: Float
turretRoF :: Float
bruteRoF :: Float
swarmSize :: Float
turretSize :: Float
bruteSize :: Float
swarmRoF = 3
turretRoF = 10
bruteRoF = 10
swarmSize = 32
turretSize = 50
bruteSize = 50
cloudSize :: Float
cloudSize = 50
mountainSize :: Float
mountainSize = 50
planetSize :: Float
planetSize = 50

-- function for the movement of Brute and Swarm type enemies
f :: Float -> Float
f x = 3 * sin (1/(10 * 2*pi) * x)