module GameMechanics where

fps :: Int
fps = 60 

peashooterRate :: Float
peashooterRate = 10
peashooterSpeed :: Float
peashooterSpeed = 20
launcherRate :: Float
launcherRate = 2
rocketSpeed :: Float
rocketSpeed = 40

-- laser implementeren word echt lastig
-- kan het door het een lijn te maken en de frequentie van de speler gewoon naar 0 te zetten?
laserRate :: Float
laserRate = 3 -- dit is anders
laserbeamSpeed :: Float
laserbeamSpeed = 10 -- is dit nodig als het een laser die aanstaat?


xmargin :: Float
ymargin :: Float
xmargin = 400
ymargin = 200

-- definineer het bewegings pattroon van de enemies hier
-- en spawnrate
-- en de RoF
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

f :: Float -> Float
f x = 3 * sin (1/(10 * 2*pi) * x)