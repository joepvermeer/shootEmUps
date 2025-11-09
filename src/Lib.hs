module Lib
  ( World(..), Phase(..), Player(..)
  , Renderable(..), Updatable(..), Collidable(..)
  , initialWorld, drawWorldIO, handleInputIO, stepWorldIO
  , readScoresIO, top3
  ) where


import qualified Graphics.Gloss as G
import Graphics.Gloss.Interface.IO.Game ( Event(..), Key(..), SpecialKey(..), KeyState(..) )
import System.Exit (exitSuccess)
import Data.List (partition, foldl', sortOn)
import System.Random (StdGen, randomR, split)
import System.Directory (doesFileExist)
import System.IO (appendFile)
import qualified Data.Ord as Ord
import Text.Read (readMaybe)


--constants
winW, winH :: Float
winW = 1280
winH = 760

--constants for explosion 
explosionTTL, explosionRadius :: Float
explosionTTL   = 0.4
explosionRadius = 18

--constants for player speed and radius
playerRadius, playerSpeed :: Float
playerRadius = 10
playerSpeed  = 300

-- constants for bullets
bulletSpeed, bulletRadius, fireCooldown :: Float
bulletSpeed  = 600
bulletRadius = 4
fireCooldown = 0.4

--enemy radius constant. let op: sommige dingen zijn relatief hieraan dus niet snel veranderen 
enemyRadius      :: Float; enemyRadius      = 12

--chaser constants
spawnMarginX     :: Float; spawnMarginX     = 20  -- begin buiten beeld met spawns
chaserSpeedX     :: Float; chaserSpeedX     = 180 
chaserSpeedY     :: Float; chaserSpeedY     = 160 
chaserHP         :: Int;   chaserHP         = 2

-- schade 
bulletDamage   :: Int;   bulletDamage   = 1
touchDamage    :: Int;   touchDamage    = 1
playerMaxHP    :: Int;   playerMaxHP    = 3

--immuniteit
iFrameDuration :: Float; iFrameDuration = 2

--random spawns
spawnMin, spawnMax :: Float
spawnMin = 0.6     
spawnMax = 1.0    

--zigzag enemy 
zigzagSpeedX    :: Float; zigzagSpeedX    = 180    -- naar links
zigzagAmpSpeed  :: Float; zigzagAmpSpeed  = 280    -- px/s op-en-neer
zigzagOmega     :: Float; zigzagOmega     = 2 * pi / 1.2

--tank 
tankRadius  :: Float; tankRadius  = enemyRadius * 2
tankSpeedX  :: Float; tankSpeedX  = 60
tankHP      :: Int;   tankHP      = 5

--turret 
turretRadius       :: Float; turretRadius       = enemyRadius
turretHP           :: Int;   turretHP           = 1
turretFireCooldown :: Float; turretFireCooldown = 3    -- schiet cooldown 
enemyBulletSpeed   :: Float; enemyBulletSpeed   = 220
enemyBulletRadius  :: Float; enemyBulletRadius  = 4
enemyBulletDamage  :: Int;   enemyBulletDamage  = 1

--speeder 
speederSpeedX        :: Float; speederSpeedX        = 120 -- normale snelheid speeder
speederDashSpeedX    :: Float; speederDashSpeedX    = 480   -- dash naar voren
speederDashMaxVy     :: Float; speederDashMaxVy     = 420   -- max verticale snelheid tijdens dash
speederAimGain       :: Float; speederAimGain       = 6.0   -- hoe agressief Y naar speler trekt
speederDashDuration  :: Float; speederDashDuration  = 0.35
speederDashCooldown  :: Float; speederDashCooldown  = 1.8

--power ups 
powerupSize      :: Float; powerupSize      = 18
powerupSpeedX    :: Float; powerupSpeedX    = 90      -- drijven langzaam mee naar links
powerupSpawnMin  :: Float; powerupSpawnMin  = 8       -- tussen 8 en 14 s
powerupSpawnMax  :: Float; powerupSpawnMax  = 14
rapidFireDuration:: Float; rapidFireDuration= 6       -- seconden 2x sneller schieten

--classes
class Renderable a where
  render :: a -> G.Picture

class Updatable a where
  update :: Float -> a -> a

class Collidable a where
  hitCircle :: a -> (Float, Float, Float) -- (cx, cy, r)


--domain types
data Phase = StartName | StartReady | Running | Paused | GameOver
  deriving (Eq, Show)

data Player = Player
  { px :: Float
  , py :: Float
  , vx :: Float
  , vy :: Float
  , pr :: Float   
  , cooldown :: Float 
  , firing :: Bool
  , php :: Int
  , ifr :: Float
  } deriving (Show)

data Bullet = Bullet
  { bx :: Float
  , by :: Float
  , bvx :: Float
  , bvy :: Float
  , br  :: Float
  } deriving (Show)

data EnemyBullet = EnemyBullet
  { ebx :: Float
  , eby :: Float
  , ebvx :: Float
  , ebvy :: Float
  , ebr  :: Float
  } deriving (Show)

data World = World
  { player  :: Player
  , phase   :: Phase
  , fieldW  :: Float
  , fieldH  :: Float
  , speed   :: Float  
  , bullets :: [Bullet]
  , enemies :: [Enemy]
  , rng     :: StdGen
  , spawnTimer :: Float
  , score   :: Int
  , explosions :: [Explosion]
  , enemyBullets :: [EnemyBullet]
  , playerName  :: String        -- kan leeg zijn 
  , highscores  :: [(String,Int)]
  , powerups       :: [Powerup]
  , powerupTimer   :: Float        -- tijd tot volgende spawn 
  , rapidFireTimer :: Float        
  } deriving (Show)

--enemies 
data EnemyKind = Chaser | Zigzag | Turret | Tank | Speeder
  deriving (Eq, Show)

data Enemy = Enemy
  { ex   :: Float
  , ey   :: Float
  , evx  :: Float
  , evy  :: Float
  , er   :: Float
  , ehp  :: Int
  , ekind :: EnemyKind
  , eaux  :: Float   -- extra state (bv. fase/angle voor zigzag), nu 0.0
  } deriving (Show)

data Explosion = Explosion
  { exx  :: Float
  , exy  :: Float
  , ettl :: Float
  } deriving (Show)

--data types for powerups 
data PowerupKind = PUExtraLife | PURapidFire
  deriving (Eq, Show)

data Powerup = Powerup
  { pux :: Float
  , puy :: Float
  , puk :: PowerupKind
  } deriving (Show)

--instances for domain types

instance Renderable Player where
  render p =
    let base = G.color G.white (G.circleSolid (pr p))
        flash = if ifr p > 0
                then G.color (G.makeColor 1 1 0 0.5) (G.circle (pr p + 4))
                else G.blank
    in G.translate (px p) (py p) (G.pictures [base, flash])

--instance voor bullet 
instance Renderable Bullet where
  render b =
    G.translate (bx b) (by b) $
      G.color (G.greyN 0.9) (G.circleSolid (br b))

-- instances for explosion 
instance Renderable Explosion where
  render e =
    let a = max 0 (min 1 (ettl e / explosionTTL))  -- 1 -> 0
        pic = G.circleSolid (explosionRadius * (1 + (1 - a)))  -- groeit
    in G.translate (exx e) (exy e)
       $ G.color (G.makeColor 1 0.7 0 a)
       $ pic

instance Updatable Explosion where
  update dt e = e { ettl = ettl e - dt }

--instance voor enemy
instance Renderable Enemy where
  render e =
    let col = case ekind e of
                Chaser  -> G.makeColorI 255 90 90 255
                Zigzag  -> G.makeColorI 255 200 60 255
                Turret  -> G.makeColorI 200 200 255 255
                Tank    -> G.makeColorI 120 200 120 255
                Speeder -> if eaux e < 0
                             then G.makeColorI 255 105 180 255 -- kleur van dash 
                             else G.makeColorI 200 0 255 255 -- normale kleur speeder
    in G.translate (ex e) (ey e) $
         G.color col (G.circleSolid (er e))

instance Updatable Bullet where
  update dt b = b { bx = bx b + bvx b * dt
                  , by = by b + bvy b * dt }

instance Updatable Player where
  update dt p = p { px = px p + vx p * dt
                  , py = py p + vy p * dt }

instance Collidable Player where
  hitCircle p = (px p, py p, pr p)


instance Renderable EnemyBullet where
  render b =
    G.translate (ebx b) (eby b) $
      G.color (G.makeColorI 255 140 0 255) (G.circleSolid (ebr b))

instance Updatable EnemyBullet where
  update dt b = b { ebx = ebx b + ebvx b * dt
                  , eby = eby b + ebvy b * dt }

--instance for power ups 
instance Renderable Powerup where
  render p =
    let sz  = powerupSize
        col = case puk p of
                PUExtraLife -> G.makeColorI 255 0 0 255    
                PURapidFire -> G.makeColorI 0 160 255 255   
    in G.translate (pux p) (puy p)
       $ G.color col
       $ G.rectangleSolid sz sz

instance Updatable Powerup where
  update dt p = p { pux = pux p - powerupSpeedX * dt }


-- Initialisatie van de wereld op begin spel 

initialWorld :: StdGen -> World
initialWorld g =
  let r = playerRadius
      margin = 30
  in World
      { player = Player
          { px = (-winW / 2) + r + margin
          , py = 0
          , vx = 0, vy = 0
          , pr = r
          , cooldown = 0
          , firing = False
          , php = playerMaxHP
          , ifr = 0
          }
      , phase      = StartName
      , fieldW     = winW
      , fieldH     = winH
      , speed      = playerSpeed
      , bullets    = []
      , enemies    = []
      , rng        = g
      , spawnTimer = 0.5
      , score      = 0
      , explosions = []
      , enemyBullets = []
      , playerName = ""
      , highscores = []
      , powerups       = []
      , powerupTimer   = 10             
      , rapidFireTimer = 0
      }

-- helper voor herstarten 
resetWorld :: World -> World
resetWorld w =
  let g  = rng w
      hs = highscores w
      w0 = initialWorld g
  in w0 { highscores = hs        -- behoud top3 in geheugen
        , phase      = StartName -- terug naar naam invoeren
        , playerName = ""        -- naam leegmaken
        , score      = 0
        }



-- Tekenen van de hele wereld.
drawWorldIO :: World -> IO G.Picture
drawWorldIO w = pure $ G.pictures
  [ render (player w)
  , G.pictures (map render (bullets w))
  , G.pictures (map render (enemies w))
  , hud w
  , uiOverlay w
  , G.pictures (map render (explosions w))
  , G.pictures (map render (enemyBullets w))
  , scoreboard w
  , G.pictures (map render (powerups w)) 
  ]


scoresFile :: FilePath
scoresFile = "scores.txt"   -- file om scores bij te houden. wordt automatisch aangemaakt nu 

appendScoreIO :: String -> Int -> IO ()
appendScoreIO name sc = appendFile scoresFile (name <> "," <> show sc <> "\n")

--scores.txt openen en inlezen om het te gebruiken in de HUD 
readScoresIO :: IO [(String,Int)]
readScoresIO = do
  ok <- doesFileExist scoresFile
  if not ok then pure [] else do
    txt <- readFile scoresFile
    let parseLine ln = case break (==',') ln of
          (nm, ',':rest) -> case readMaybe rest of
                              Just n  -> Just (nm, n)
                              Nothing -> Nothing
          _              -> Nothing
        xs = mapMaybe parseLine (lines txt)
    pure xs
  where
    mapMaybe f = foldr (\a acc -> maybe acc (:acc) (f a)) []

--beste scores sorteren en top 3 uitrekenen 
top3 :: [(String,Int)] -> [(String,Int)]
top3 = take 3 . sortOn (Ord.Down . snd)

-- Input handelen 
handleInputIO :: Event -> World -> IO World
handleInputIO ev w = case ev of
  -- naam invoer 
  -- backspace werkt niet op MacOs voor een of andere reden. 
  EventKey (SpecialKey KeyBackspace) Down _ _ | phase w == StartName -> --normale backspace 
    let s = playerName w
    in pure w { playerName = if null s then s else init s }

  EventKey (Char '\b') Down _ _ | phase w == StartName ->  -- ASCII backspace
    let s = playerName w
    in pure w { playerName = if null s then s else init s }

  EventKey (Char '\DEL') Down _ _ | phase w == StartName -> -- ASCII 127
    let s = playerName w
    in pure w { playerName = if null s then s else init s }

  EventKey (SpecialKey KeyEnter) Down _ _ | phase w == StartName ->
    pure w { phase = StartReady }

  EventKey (SpecialKey KeyEnter) Down _ _ | phase w == StartName ->
    pure w { phase = StartReady }

  EventKey (Char c) Down _ _ | phase w == StartName ->
    -- alleen ASCII letters die je kan zien en max lengte van 20, anders te lang voor score board 
    let ok = c >= ' ' && c <= '~'
        s  = playerName w
    in pure $ if ok && length s < 20
              then w { playerName = s <> [c] }
              else w

  -- Starten
  EventKey (Char 's') Down _ _ | phase w == StartReady ->
    pure (w { phase = Running })
  -- Pauze togglen met 'p'. kan alleen in Running of Paused
  EventKey (Char 'p') Down _ _ ->
    pure $ case phase w of
             Running -> w { phase = Paused }
             Paused  -> w { phase = Running }
             _       -> w

  -- Restart met 'r' vanuit elke fase
  EventKey (Char 'r') Down _ _ -> pure (resetWorld w)
  -- input voor pijltjes, misschien combinatie toevoegen voor diagonaal bewegen? 
  --X-as 
  EventKey (SpecialKey KeyLeft)  Down _ _ -> pure (pressLeft  w)
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (pressRight w)
  EventKey (SpecialKey KeyLeft)  Up   _ _ -> pure (releaseLeft  w)
  EventKey (SpecialKey KeyRight) Up   _ _ -> pure (releaseRight w)

  -- Y-as
  EventKey (SpecialKey KeyUp)    Down _ _ -> pure (pressUp    w)
  EventKey (SpecialKey KeyDown)  Down _ _ -> pure (pressDown  w)
  EventKey (SpecialKey KeyUp)    Up   _ _ -> pure (releaseUp    w)
  EventKey (SpecialKey KeyDown)  Up   _ _ -> pure (releaseDown  w)

  
  --schieten spatiebalk 
  EventKey (Char ' ')            Down _ _ -> pure (setFiring True  w)
  EventKey (Char ' ')            Up   _ _ -> pure (setFiring False w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (setFiring True  w)
  EventKey (SpecialKey KeySpace) Up   _ _ -> pure (setFiring False w)

--voor testen, makkelijk sluiten met esc 
  EventKey (SpecialKey KeyEsc) Down _ _ -> exitSuccess
  _ -> pure w
  
-- huidige bewegingssnelheid uit de wereld
moveSpeed :: World -> Float
moveSpeed w = speed w

--hulp functies voor bewegen 
setVX :: Float -> World -> World
setVX v world = world { player = (player world) { vx = v } }

setVY :: Float -> World -> World
setVY v world = world { player = (player world) { vy = v } }

pressLeft :: World -> World
pressLeft  w = setVX (-(moveSpeed w)) w

pressRight :: World -> World
pressRight w = setVX (  (moveSpeed w)) w

pressUp :: World -> World
pressUp    w = setVY (  (moveSpeed w)) w

pressDown :: World -> World
pressDown  w = setVY (-(moveSpeed w)) w

releaseLeft :: World -> World
releaseLeft  w =
  let p = player w in if vx p < 0 then setVX 0 w else w

releaseRight :: World -> World
releaseRight w =
  let p = player w in if vx p > 0 then setVX 0 w else w

releaseUp :: World -> World
releaseUp    w =
  let p = player w in if vy p > 0 then setVY 0 w else w

releaseDown :: World -> World
releaseDown  w =
  let p = player w in if vy p < 0 then setVY 0 w else w

--hulp functies bewegen, lopen tm hier

--hulpfinctie voor schieten
fireBullet :: World -> World
fireBullet w =
  let p  = player w
      cd = cooldown p
      cdReset = 0.2       -- cooldown tijd in seconden, kunnen we aanpassen voor sneller schieten (powerups misschien?)
      r  = pr p
      startX = px p + r + 2
      startY = py p
      v = 600              -- snelheid kogels
      b = Bullet { bx = startX, by = startY, bvx = v, bvy = 0, br = 4 }
  in if cd <= 0
       then w { player = p { cooldown = cdReset }
              , bullets = b : bullets w }
       else w

setFiring :: Bool -> World -> World
setFiring on world =
  let p = player world
  in world { player = p { firing = on } }

--maak kogel vanuit speler en gebruik de constants
mkBulletFrom :: Player -> Bullet
mkBulletFrom p =
  let startX = px p + pr p + 2
      startY = py p
  in Bullet
       { bx = startX
       , by = startY
       , bvx = bulletSpeed
       , bvy = 0
       , br  = bulletRadius
       }

--enemies 
spawnChaserAt :: Float -> Enemy
spawnChaserAt y =
  Enemy { ex    =  winW/2 + enemyRadius + spawnMarginX
        , ey    =  clampY y
        , evx   = -chaserSpeedX
        , evy   =  0
        , er    =  enemyRadius
        , ehp   =  chaserHP
        , ekind =  Chaser
        , eaux  =  0
        }
  where
    -- niet buiten speelveld spawnen op y-as
    clampY yy = max (-winH/2 + enemyRadius) (min (winH/2 - enemyRadius) yy)

spawnTankAt :: Float -> Enemy
spawnTankAt y =
  Enemy { ex    =  winW/2 + tankRadius + spawnMarginX
        , ey    =  max (-winH/2 + tankRadius) (min (winH/2 - tankRadius) y)
        , evx   = -tankSpeedX
        , evy   =  0
        , er    =  tankRadius
        , ehp   =  tankHP
        , ekind =  Tank
        , eaux  =  0
        }

spawnTurretAt :: Float -> Enemy
spawnTurretAt y =
  Enemy
    { ex    =  winW/2 - turretRadius - 30      -- in beeld, rechts
    , ey    =  clampY y
    , evx   =  0
    , evy   =  0
    , er    =  turretRadius
    , ehp   =  turretHP
    , ekind =  Turret
    , eaux  =  turretFireCooldown               -- start met cooldown 
    }
  where
    clampY yy = max (-winH/2 + turretRadius) (min (winH/2 - turretRadius) yy)

--spawn speeders
spawnSpeederAt :: Float -> Enemy
spawnSpeederAt y =
  Enemy
    { ex    = winW/2 + enemyRadius + spawnMarginX
    , ey    = max (-winH/2 + enemyRadius) (min (winH/2 - enemyRadius) y)
    , evx   = -speederSpeedX
    , evy   = 0
    , er    = enemyRadius
    , ehp   = 1
    , ekind = Speeder
    , eaux  = 0    -- start: mag dashen
    }

-- helper voor spawn powerups
spawnPowerupAt :: PowerupKind -> Float -> Powerup
spawnPowerupAt k y =
  Powerup { pux =  fieldW0/2 + powerupSize + spawnMarginX
          , puy =  clampY y
          , puk =  k
          }
  where
    -- gebruik vaste window grootte
    fieldW0 = winW
    fieldH0 = winH
    clampY yy = max (-fieldH0/2 + powerupSize) (min (fieldH0/2 - powerupSize) yy)

--volgende spawntijd random en random y locatie voor spawn 
nextPowerupSpawn :: StdGen -> (Float, Float, StdGen)
nextPowerupSpawn g0 =
  let (t, g1) = randomR (powerupSpawnMin, powerupSpawnMax) g0
      (y, g2) = randomR (-winH/2 + powerupSize, winH/2 - powerupSize) g1
  in (t, y, g2)

-- kies powerupsoort met kans 50/50
pickPowerup :: StdGen -> (PowerupKind, StdGen)
pickPowerup g =
  let (i, g') = randomR (0 :: Int, 1) g
  in (if i == 0 then PUExtraLife else PURapidFire, g')

-- pakken powerup op als circkel ipv vierkant. voor gameplay werkt het nagenoeg het zelfde 
powerupCircle :: Powerup -> (Float,Float,Float)
powerupCircle p = (pux p, puy p, powerupSize * 0.8)

resolvePowerups :: Player -> [Powerup]
                -> (Player, [Powerup], Bool, Maybe PowerupKind)
resolvePowerups p ps =
  case break (\pu -> circleHit (playerCircle p) (powerupCircle pu)) ps of
    (left, pu:right) ->
      -- gepakt: verwijder pu, pas player toe
      let (p', effectApplied) = applyPowerup p pu
      in (p', left ++ right, True, Just (puk pu))
    _ -> (p, ps, False, Nothing)


applyPowerup :: Player -> Powerup -> (Player, ())
applyPowerup p pu =
  case puk pu of
    PUExtraLife ->
      ( p { php = min playerMaxHP (php p + 1) }, () )
    PURapidFire ->
      -- timer wordt op world gezet; player zelf hoeft niet te veranderen
      ( p, () )

--huidige fire cooldown, halveer rapidfire (dan is firerate weer normaal)
currentFireCooldown :: World -> Float
currentFireCooldown w =
  if rapidFireTimer w > 0 then fireCooldown / 2 else fireCooldown

--handel schieten en botsingen met enemies 
circleHit :: (Float,Float,Float) -> (Float,Float,Float) -> Bool
circleHit (x1,y1,r1) (x2,y2,r2) =
  let dx = x1 - x2; dy = y1 - y2
  in dx*dx + dy*dy <= (r1 + r2) * (r1 + r2)

bulletCircle :: Bullet -> (Float,Float,Float)
bulletCircle b = (bx b, by b, br b)

enemyCircle :: Enemy -> (Float,Float,Float)
enemyCircle e = (ex e, ey e, er e)

playerCircle :: Player -> (Float,Float,Float)
playerCircle p = (px p, py p, pr p)

--helper om verslaan van enemies verschillende scores te geven. 
scoreFor :: Enemy -> Int
scoreFor e = case ekind e of
  Chaser  -> 1
  Zigzag  -> 1
  Tank    -> 3
  Turret  -> 3   
  Speeder -> 2           

--spawn nieuewe enemy na tijd 
nextSpawn :: StdGen -> (Float, Float, StdGen)
nextSpawn g0 =
  let (t, g1) = randomR (spawnMin, spawnMax) g0
      (y, g2) = randomR (-winH/2 + enemyRadius, winH/2 - enemyRadius) g1
  in (t, y, g2)

updateEnemy :: Float -> Float -> Enemy -> Enemy
updateEnemy dt playerY e =
  case ekind e of
    Chaser ->
      let dy   = playerY - ey e
          aimV = clamp (-chaserSpeedY) chaserSpeedY (dy / max 1e-3 dt)
          -- beweeg
          e' = e { ex = ex e + evx e * dt
                 , ey = ey e + aimV * dt
                 }
      in e'
    Zigzag ->
      let ph   = eaux e + zigzagOmega * dt
          vy   = zigzagAmpSpeed * sin ph
      in e { ex = ex e + evx e * dt
           , ey = ey e + vy * dt
           , eaux = ph
           }
    Turret -> e { eaux = max 0 (eaux e - dt) }
    Tank   -> e { ex = ex e + evx e * dt } 
    Speeder -> e { ex = ex e + evx e * dt }   -- simpele voortbeweging
  where
    clamp a b v = max a (min b v)

--helper voor schieten van Turret 
shootTurrets :: Player -> [Enemy] -> ([Enemy], [EnemyBullet])
shootTurrets p = foldr step ([], [])
  where
    step e (accE, accB) =
      case ekind e of
        Turret | eaux e <= 0 ->
          let (bx, by) = (ex e, ey e)
              (tx, ty) = (px p, py p)
              (dx, dy) = (tx - bx, ty - by)
              len      = max 1e-3 (sqrt (dx*dx + dy*dy))
              (ux, uy) = (dx / len, dy / len)
              b = EnemyBullet { ebx = bx, eby = by
                              , ebvx = ux * enemyBulletSpeed
                              , ebvy = uy * enemyBulletSpeed
                              , ebr  = enemyBulletRadius
                              }
              e' = e { eaux = turretFireCooldown }
          in (e' : accE, b : accB)  -- 
        _ -> (e : accE, accB)

--helper voor turret bullets
enemyBulletCircle :: EnemyBullet -> (Float,Float,Float)
enemyBulletCircle b = (ebx b, eby b, ebr b)

--helper voor sprints van speeder
updateSpeeder :: Float -> Float -> Enemy -> Enemy
updateSpeeder dt playerY e
  | ekind e /= Speeder = e
  | otherwise =
      let t = eaux e - dt
      in if t > 0
           then
             -- cooldown: horizontaal bewegen; evy snel naar 0 (failsafe)
             let vy0   = evy e
                 rate  = speederRelaxRate        
                 step  = max (-rate*dt) (min (rate*dt) (0 - vy0))
                 vy1   = vy0 + step
             in e { ex = ex e + evx e * dt
                  , ey = ey e + vy1 * dt
                  , evy = vy1
                  , eaux = t
                  }
           else if t > -speederDashDuration
             then
               -- dash actief naar speler 
               let dy   = playerY - ey e
                   vy0  = evy e
                   vy1  = vy0 + speederAimGain * dy
                   vy'  = max (-speederDashMaxVy) (min speederDashMaxVy vy1)
                   vx'  = -speederDashSpeedX
               in e { ex   = ex e + vx' * dt
                    , ey   = ey e + vy' * dt
                    , evy  = vy'              -- onthoud verticale snelheid tijdens dash
                    , eaux = t
                    }
           else
             -- na dash direct weer horizontaal bewegen 
             e { eaux = speederDashCooldown
               , ex   = ex e + evx e * dt
               , ey   = ey e                  -- geen extra verticale verplaatsing
               , evy  = 0                     -- reset verticale snelheid
               }

--helper voor horzinontaal hervatten van de speeder
speederRelaxRate :: Float
speederRelaxRate = 800  

-- helper voor zigzag enemy data type 
spawnZigzagAt :: Float -> Enemy
spawnZigzagAt y =
  Enemy { ex    =  winW/2 + enemyRadius + spawnMarginX
        , ey    =  max (-winH/2 + enemyRadius) (min (winH/2 - enemyRadius) y)
        , evx   = -zigzagSpeedX
        , evy   =  0
        , er    =  enemyRadius
        , ehp   =  1
        , ekind =  Zigzag
        , eaux  =  0       
        }

--enemy raakt player, houdt bij dat hij onschadelijk is tijdelijk of dat hij game over is 
resolveEnemyPlayer :: [Enemy] -> Player -> (Player, Bool)
resolveEnemyPlayer es p
  | ifr p > 0  = (p, False)  -- onkwetsbaar
  | otherwise  =
      if any (\e -> circleHit (enemyCircle e) (playerCircle p)) es
        then ( p { php = php p - touchDamage, ifr = iFrameDuration }, True )
        else ( p, False )

-- botsingen tussen bullets en enemies oplossen, return nieuwe lijsten en score toename
-- explosie voor als een bullet een enemie raakt 
--handelt ook score voor als je een enemy verslaat
resolveBulletEnemy :: [Enemy] -> [Bullet] -> ([Bullet],[Enemy], Int, [Explosion])
resolveBulletEnemy enemies bullets = go 0 [] [] enemies bullets
  where
    go :: Int -> [Enemy] -> [Explosion] -> [Enemy] -> [Bullet]
       -> ([Bullet],[Enemy], Int, [Explosion])
    go sc keptEs exps []     bs = (bs, reverse keptEs, sc, exps)
    go sc keptEs exps (e:es) bs =
      let (hitBs, restBs) = partition (\b -> circleHit (bulletCircle b) (enemyCircle e)) bs
          e'    = foldl' (\acc _ -> damageEnemy bulletDamage acc) e hitBs
          dead  = ehp e' <= 0
          sc'   = if dead then sc + scoreFor e else sc
          exps' = if dead
                    then Explosion { exx = ex e, exy = ey e, ettl = explosionTTL } : exps
                    else exps
          kept' = if dead then keptEs else (e' : keptEs)
      in go sc' kept' exps' es restBs

--verwijderd alle enemy bullets die raken 
resolveEnemyBulletsPlayer :: [EnemyBullet] -> Player -> (Player, [EnemyBullet], [Explosion], Bool)
resolveEnemyBulletsPlayer ebs p =
  let pc    = playerCircle p
      (hit, keep) = partition (\b -> circleHit (enemyBulletCircle b) pc) ebs
  in if null hit
       then (p, ebs, [], False)
       else
         let took = if ifr p <= 0 then True else False
             p' = if took
                    then p { php = php p - enemyBulletDamage
                           , ifr = iFrameDuration }
                    else p
             boom = if took
                      then [Explosion { exx = fst3 pc, exy = snd3 pc, ettl = explosionTTL }]
                      else []
         in (p', keep, boom, took)
  where
    fst3 (a,_,_) = a
    snd3 (_,b,_) = b


resolveEnemyPlayerConsume :: [Enemy] -> Player -> (Player, [Enemy], [Explosion], Bool)
resolveEnemyPlayerConsume es p
  | ifr p > 0 = (p, es, [], False)
  | otherwise =
      go [] es
  where
    pc = playerCircle p
    go kept []     = (p, reverse kept, [], False)
    go kept (e:rs) =
      if circleHit (enemyCircle e) pc
        then
          let p' = p { php = php p - touchDamage, ifr = iFrameDuration}
              boom = Explosion { exx = ex e, exy = ey e, ettl = explosionTTL }
          in (p', reverse kept ++ rs, [boom], True) -- enemy e verdwijnt
        else
          go (e:kept) rs

updateExplosions :: Float -> [Explosion] -> [Explosion]
updateExplosions dt = filter (\e -> ettl e > 0) . map (update dt)


--enemy gaat leven van af als hij is geraakt 
damageEnemy :: Int -> Enemy -> Enemy
damageEnemy d e = e { ehp = ehp e - d }

-- Tijdstap in de wereld 
stepWorldIO :: Float -> World -> IO World
stepWorldIO dt w =
  case phase w of
    StartName  -> pure w
    StartReady -> pure w
    Paused     -> pure w
    GameOver   -> pure w
    Running    ->
      let
        -- speler updaten en cooldown omlaag
        p1 = update dt (player w)
        p2 = p1 { cooldown = max 0 (cooldown p1 - dt) }

        -- binnen window houden
        halfW = fieldW w / 2 - pr p2
        halfH = fieldH w / 2 - pr p2
        clampX x = max (-halfW) (min halfW x)
        clampY y = max (-halfH) (min halfH y)
        p3 = p2 { px = clampX (px p2), py = clampY (py p2) }

        -- auto-fire: als firing aan staat en cooldown vrij is
        (p4, fired) =
          if firing p3 && cooldown p3 <= 0
            then ( p3 { cooldown = currentFireCooldown w }, [mkBulletFrom p3] )
            else ( p3, [] )

        -- bullets updaten en buiten beeld weggooien
        bs'  = map (update dt) (bullets w ++ fired)
        outL = -fieldW w / 2
        outR =  fieldW w / 2
        outB = -fieldH w / 2
        outT =  fieldH w / 2
        inBoundsB b = let x = bx b; y = by b
                      in x >= outL && x <= outR && y >= outB && y <= outT
        bs'' = filter inBoundsB bs'

        -- powerups aftellen timers en evt spawn 
        puT1 = powerupTimer w - dt
        (wPUSpawned, puNext) =
          if puT1 <= 0
            then
              let (k, g1)      = pickPowerup (rng w)
                  (tNew, yPU, g2) = nextPowerupSpawn g1
                  puNew          = spawnPowerupAt k yPU
              in ( w { powerups = puNew : powerups w, rng = g2 }, tNew )
            else (w, puT1)

        -- spawn enemy timer
        t1 = spawnTimer wPUSpawned - dt

        -- eventueel spawnen enemy kinds en timer resetten
        (wSpawned0, tNext) =
          if t1 <= 0
            then
              let (tNew, ySpawn, g1) = nextSpawn (rng wPUSpawned)
                  (pick, g2)         = randomR (0 :: Int, 4) g1 --spawnt een random enemy aan de hand van nummer dat wordt gekozen 
                  eNew = case pick of
                           0 -> spawnChaserAt ySpawn
                           1 -> spawnZigzagAt ySpawn
                           2 -> spawnTankAt   ySpawn
                           3 -> spawnTurretAt ySpawn
                           _ -> spawnSpeederAt ySpawn
              in ( wPUSpawned { enemies = eNew : enemies wPUSpawned, rng = g2 }, tNew )
            else (wPUSpawned, t1)

        -- enemies updaten (per type)
        pyPlayer = py p4
        esTicked = map (updateEnemy dt pyPlayer) (enemies wSpawned0)

        -- turrets laten schieten en cooldown resetten
        (esShot, newEBullets) = shootTurrets p4 esTicked

        -- speeders: dash-gedrag toepassen
        esDashed = map (updateSpeeder dt pyPlayer) esShot

        -- enemies links buiten beeld verwijderen
        leftBound = -fieldW w / 2 - enemyRadius - spawnMarginX
        es'' = filter (\e -> ex e > leftBound) esDashed

        -- powerupos updaten en buiten beeld verwijderen 
        pus'  = map (update dt) (powerups wSpawned0)
        leftCullPU = -fieldW w / 2 - powerupSize - spawnMarginX
        inBoundsPU p = let x = pux p; y = puy p
               in x >= leftCullPU && y >= outB && y <= outT
        pus'' = filter inBoundsPU pus'

        -- collisions afhandelen

        -- bullets vs enemies en explosie bij death 
        (bsAfter, esAfterBullets, gained, boomFromPBullets) =
          resolveBulletEnemy es'' bs''

        -- enemy bodies vs player 
        (p5, esAfterBodies, boomFromBodies, _tookEnemy) =
          resolveEnemyPlayerConsume esAfterBullets p4

        -- enemy bullets updaten en cullen (van turrets)
        ebs'  = map (update dt) (enemyBullets w ++ newEBullets)
        inBoundsEB b = let x = ebx b; y = eby b
                       in x >= outL && x <= outR && y >= outB && y <= outT
        ebs'' = filter inBoundsEB ebs'

        -- enemy bullets vs player
        (p6, ebsAfter, boomFromEB, _tookEB) =
          resolveEnemyBulletsPlayer ebs'' p5

        -- powerup opgepakt
        (p7, pusAfter, picked, whichPU) = resolvePowerups p6 pus''

        -- rapid fire timer updaten 
        rf' = max 0 (rapidFireTimer w - dt)
        rf'' = case whichPU of
                 Just PURapidFire -> max rf' rapidFireDuration
                 _                -> rf'
        -- extra life is al in player toegepast

        -- i-frames tikken omlaag
        p8       = p7 { ifr = max 0 (ifr p7 - dt) }
        newScore = score w + gained

        -- explosions updaten en toevoegen
        exps'  = explosions w ++ boomFromPBullets ++ boomFromBodies ++ boomFromEB
        exps'' = updateExplosions dt exps'

        -- game over check
        phase' = if php p8 <= 0 then GameOver else Running

        -- pure wereld na deze tick
        wOutPure = wSpawned0
          { player         = p8
          , bullets        = bsAfter
          , enemyBullets   = ebsAfter
          , enemies        = esAfterBodies
          , powerups       = pusAfter
          , powerupTimer   = puNext
          , rapidFireTimer = rf''
          , spawnTimer     = tNext
          , explosions     = exps''
          , score          = newScore
          , phase          = phase'
          }
      in do
        -- Als we net doodgingen score opslaan en top 3 bijwerken 
        if phase w == Running && phase' == GameOver
          then do
            let nm = if null (playerName w) then "anon" else playerName w
            appendScoreIO nm (score wOutPure)
            xs <- readScoresIO
            pure wOutPure { highscores = top3 xs }
          else
            pure wOutPure



-- simpele HUD, kan nog wel veranderen 
hud :: World -> G.Picture
hud w = G.pictures
  [ G.translate (-fieldW w/2 + 10) (fieldH w/2 - 30)
      $ G.scale 0.12 0.12
      $ G.color G.white
      $ G.text ("HP:" <> show (php (player w)) <> "  SCORE:" <> show (score w))
      , if rapidFireTimer w > 0
      then G.translate 0 (-40) $ G.scale 0.1 0.1 $ G.color G.cyan $ G.text "RAPID"
      else G.blank
  ]

-- UI overlay voor start, pauze en game over
uiOverlay :: World -> G.Picture
uiOverlay w = case phase w of
  StartName ->
    bannerLines
      [ "Vul je naam in:"
      , playerName w <> "_"
      , ""
      , "Druk op enter om te bevestigen"
      ]
  StartReady ->
    bannerLines
      [ "Naam: " <> playerName w
      , "Druk op S om te beginnen"
      ]
  Paused ->
    banner "Pauze (P hervatten, R herstart)"
  GameOver ->
    banner ("Game Over  (R herstart)  |  Score: " <> show (score w))
  Running -> G.blank
  where
    banner msg =
      G.translate (-winW/3) 0
      $ G.scale 0.2 0.2
      $ G.color (G.makeColor 1 1 1 0.85)
      $ G.text msg

    bannerLines ls =
      G.translate (-winW/3) 80
      $ G.color (G.makeColor 1 1 1 0.85)
      $ G.pictures
          [ G.translate 0 (-40 * fromIntegral i)
              (G.scale 0.2 0.2 (G.text t))
          | (i,t) <- zip [0..] ls
          ]


--score board van top 3 laten zien rechts boven in window 
scoreboard :: World -> G.Picture
scoreboard w =
  let items = zip [1..] (top3 (highscores w))
      baseX = fieldW w/2 - 240
      baseY = fieldH w/2 - 40
      line (i,(nm,sc)) =
        G.translate baseX (baseY - 24 * fromIntegral (i-1)) $
          G.scale 0.12 0.12 $
            G.color G.white $
              G.text (show i <> ". " <> nm <> " - " <> show sc)
  in G.pictures (map line items)
