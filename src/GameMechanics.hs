module GameMechanics where

fps :: Int
fps = 60 
-- plaats om alle variabelen te dumpen
-- dit is een prima manier, dit is de b van y = ax + b wat handig is voor aanpassen als de score hoger wordt aangezien ik dan gewoon de formule kan intypen in de methode
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




-- definineer het bewegings pattroon van de enemies hier
-- en spawnrate
-- en de RoF
swarmRoF :: Float
turretRoF :: Float
wormRoF :: Float
swarmSize :: Float
turretSize :: Float
wormSize :: Float
swarmRoF = 3
turretRoF = 10
wormRoF = 10
swarmSize = 32
turretSize = 50
wormSize = 50
cloudSize = 50
mountainSize = 50

f :: Float -> Float
f x = 3 * sin (1/(10 * 2*pi) * x)