{-
Copyright (C) 2015 Leon Medvinsky

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 3
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
-}

{-|
Module      : Neet.Examples.CAVE
Description : CAVE Domain
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : ghc
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Neet.Examples.CAVE (cavePlay, gameFit, simGen) where


import System.Random

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Neet

import Data.List (foldl')

import GHC.Float


deg :: Float -> Float
deg d = 2 * pi * d / 360


maxBulV :: Float
maxBulV = 1


shootInterval :: Float
shootInterval = 0.1


shipBSpeed :: Float
shipBSpeed = 1

shipBR :: Float
shipBR = 0.03


data Ship =
  Ship { shipX :: !Float -- ^ Between -1 and 1
       , shipY :: !Float -- ^ Between -1 and 1
       , shipR :: Float -- ^ Ship Radius
       , shipSpeed :: Float
       , shipTimer :: !Float
       }


data Bullet =
  Bullet { bullX :: !Float
         , bullY :: !Float
         , bullVx :: Float -- ^ Units per second
         , bullVy :: Float -- ^ Units per second
         , bullR :: Float
         }


advanceBullet :: Float -> Bullet -> Bullet
advanceBullet dt b@Bullet{..} = b { bullX = bullX + dt * bullVx
                                  , bullY = bullY + dt * bullVy
                                  }


hitShip :: Ship -> Bullet -> Bool
hitShip Ship{..} Bullet{..} = ((bullX - shipX)**2 + (bullY - shipY)**2) <= (bullR + shipR)**2


hitBoss :: Boss -> Bullet -> Bool
hitBoss Boss{..} Bullet{..} = ((bullX - bossX)**2 + (bullY - bossY)**2) <= (bullR + bossR)**2


data WavePattern =
  WavePattern { wpStartAngle :: Float
              , wpInterval   :: Float
              , wpSpacing    :: Float
              , wpVel        :: Float
              }


randomPat :: RandomGen g => g -> (WavePattern, g)
randomPat g = (WavePattern { wpStartAngle = sa
                           , wpInterval = int
                           , wpSpacing = space
                           , wpVel = v
                           }, g'''') 
  where (sa, g') = randomR (0, 2 * pi) g
        (bops, g'') = randomR (1, 5 :: Int) g'
        int = 0.05 -- fromIntegral bops * 0.05
        (space, g''') = randomR (deg 60, deg 360) g''
        (v, g'''') = randomR (0, maxBulV) g'''


patternDur :: Float
patternDur = 3


data Boss =
  Boss { bossX :: Float
       , bossY :: Float
       , bossR :: Float
       , bossP :: !WavePattern
       , bossAim :: !Float
       , patTimer :: !Float
       , intTimer :: !Float
       } 


data Control =
  Control { cX :: Float -- ^ -1 or 1
          , cY :: Float -- ^ -1 or 1
          , cShoot :: Bool
          } 


newtype Controller cont =
  Controller { control :: cont -> CaveGame cont -> (cont, Control) }


data Score =
  Score { scoreTime :: !Float
        , scoreScore :: !Int
        } 


-- | Parameter is controller type
data CaveGame cont =
  CaveGame { gameShip :: !Ship
           , gameBoss :: !Boss
           , gameSBullets :: [Bullet]
           , gameBBullets :: [Bullet]
           , gameContState :: !cont
           , gameController :: Controller cont
           , gameRand :: !StdGen
           , gameScore :: !Score
           } 


sepEither :: (a -> Either b c) -> [a] -> ([b], [c])
sepEither _ [] = ([], [])
sepEither f (x:xs) =
  let (bs, cs) = sepEither f xs
  in case f x of
      Right c -> (bs, c:cs)
      Left b -> (b:bs, cs)


bound :: Ord a => (a,a) -> a -> a
bound (mn, mx) x
  | x < mn = mn
  | x > mx = mx
  | otherwise = x

stepGame :: Float -> CaveGame cont -> Either Score (CaveGame cont)
stepGame dt !cg@CaveGame{..} =
  if gameOver
  then Left newScore
  else Right $
  CaveGame { gameShip = newShip
           , gameBoss = newBoss
           , gameSBullets = newSBulls
           , gameBBullets = newBBullets
           , gameContState = newState
           , gameController = gameController
           , gameRand = newGen
           , gameScore = newScore
           } 
  where (newState, cont) = control gameController gameContState cg
        Ship{..} = gameShip
        
        newShip = Ship { shipX = bound (-1, 1) $ shipX + dt * (cX cont) * shipSpeed
                       , shipY = bound (-1, 1) $ shipY + dt * (cY cont) * shipSpeed
                       , shipR = shipR
                       , shipSpeed = shipSpeed
                       , shipTimer = if shipTimer <= 0 then shootInterval else shipTimer - dt
                       }
        Boss{..} = gameBoss
        bossShoot = intTimer <= 0
        patDone = patTimer <= 0
        (newPat, newGen) = if patDone then randomPat gameRand else (bossP, gameRand)
        newBoss = if patDone
                  then gameBoss { bossP = newPat
                                , bossAim = wpStartAngle newPat
                                , patTimer = patternDur
                                , intTimer = wpInterval newPat
                                }
                  else gameBoss { bossP = bossP
                                , bossAim = bossAim + dt * wpSpacing bossP
                                , patTimer = patTimer - dt
                                , intTimer = if bossShoot then wpInterval bossP else intTimer - dt
                                }


        outBounds :: Bullet -> Bool
        outBounds Bullet{..} =
          bullX < (-1) - bullR ||
          bullX > 1 + bullR ||
          bullY < (-1) - bullR ||
          bullY > 1 + bullR  

        stepSBullet :: Bullet -> Either Int Bullet
        stepSBullet b@Bullet{..}
          | outBounds b        = Left 0
          | hitBoss gameBoss b = Left $ 10 + round (5 * shipY)
          | otherwise          = Right $ advanceBullet dt b

        (points, steppedSBulls) = sepEither stepSBullet gameSBullets

        shots = [ Bullet (shipX + shipR * 2) (shipY + shipR) 0 shipBSpeed shipBR
                , Bullet (shipX - shipR * 2) (shipY + shipR) 0 shipBSpeed shipBR
                ] 

        newSBulls = if shipTimer <= 0 && cShoot cont
                    then shots  ++ steppedSBulls
                    else steppedSBulls


        -- | True for game end, False for not end
        stepBBullet :: Bullet -> Either Bool Bullet
        stepBBullet b@Bullet{..}
          | outBounds b = Left False
          | hitShip gameShip b = Left True
          | otherwise = Right $ advanceBullet dt b


        (maybeHits, steppedBBullets) = sepEither stepBBullet gameBBullets

        gameOver = or maybeHits

        newBSpd = wpVel bossP

        newBBull = Bullet bossX bossY (cos bossAim * newBSpd) (sin bossAim * newBSpd) 0.02

        newBBullets = if bossShoot then newBBull : steppedBBullets else steppedBBullets

        addPoints = sum points

        Score{..} = gameScore

        newScore = Score (scoreTime + dt) (scoreScore + addPoints)


screen :: Float
screen = 800 / 2

scrInt :: Int
scrInt = 800


drawBullet :: Color -> Bullet -> Picture
drawBullet col Bullet{..} =
  Color col (Scale screen screen (Translate bullX bullY (ThickCircle bullR bullR)))


drawShip :: Ship -> Picture
drawShip Ship{..} = Color yellow (Scale screen screen (Translate shipX shipY (ThickCircle shipR shipR)))


drawBoss :: Boss -> Picture
drawBoss Boss{..} = Color red (Scale screen screen (Translate bossX bossY (ThickCircle bossR bossR)))


drawGame :: CaveGame a -> Picture
drawGame CaveGame{..} = Pictures $ [sBulPic, shipPic, bBulPic, bossPic, scorePic]
  where sBulPic = Pictures $ map (drawBullet blue) gameSBullets
        bBulPic = Pictures $ map (drawBullet green) gameBBullets
        shipPic = drawShip gameShip
        bossPic = drawBoss gameBoss
        scorePic = Translate (-200) 200 (Text (show (scoreScore gameScore)))


drawScore :: Score -> Picture
drawScore scr = Scale screen screen (Translate (-200) 200 (Text (show (scoreScore scr))))


simGloss :: Float -> Either Score (CaveGame a) -> Either Score (CaveGame a)
simGloss dt escg =
  case escg of
   Right cg -> stepGame dt cg
   Left scr -> Left scr


drawGloss :: Either Score (CaveGame a) -> Picture
drawGloss = either drawScore drawGame


data Keyboard =
  Keyboard { keyUp :: Float, keyDown :: Float, keyRight :: Float, keyLeft :: Float, keyZ :: Float }

handleEvent :: Event -> Either Score (CaveGame Keyboard) -> Either Score (CaveGame Keyboard)
handleEvent _ (Left scr) = Left scr
handleEvent e (Right cg@CaveGame{..}) = Right $ cg { gameContState = setControl e gameContState }


check :: KeyState -> Float
check Down = 1
check Up = 0


setControl :: Event -> Keyboard -> Keyboard
setControl (EventKey (SpecialKey KeyUp) st _ _) k = k { keyUp = check st }
setControl (EventKey (SpecialKey KeyDown) st _ _) k = k { keyDown = check st }
setControl (EventKey (SpecialKey KeyLeft) st _ _) k = k { keyLeft = check st }
setControl (EventKey (SpecialKey KeyRight) st _ _) k = k { keyRight = check st }
setControl (EventKey (Char 'z') st _ _) k = k { keyZ = check st }
setControl _                            k = k


initGame :: cont -> Controller cont -> CaveGame cont
initGame initCont troller =
  CaveGame { gameShip = initShip
           , gameBoss = initBoss
           , gameSBullets = []
           , gameBBullets = []
           , gameContState = initCont
           , gameController = troller
           , gameRand = gen
           , gameScore = Score 0 0
           } 
  where initBoss = Boss 0 0.5 0.2 initPat (wpStartAngle initPat) patternDur (wpInterval initPat)
        (initPat, gen) = randomPat (mkStdGen 420)
        initShip = Ship 0 (-0.5) 0.02 2 shootInterval


keyCont :: Controller Keyboard
keyCont =
  Controller $ \k@Keyboard{..} _ -> (k,Control { cX = keyRight - keyLeft
                                               , cY = keyUp - keyDown
                                               , cShoot = keyZ > 0.5
                                               })


initBoard :: Keyboard
initBoard = Keyboard 0 0 0 0 0


playerGame :: CaveGame Keyboard
playerGame = initGame initBoard keyCont


-- | Play it for yourself
cavePlay :: IO ()
cavePlay = play disp white 50 (Right playerGame) drawGloss handleEvent simGloss
  where disp = InWindow "Toohoo" (scrInt,scrInt) (0,0)


data ConcBucket =
  CB { nne :: Float
     , nee :: Float
     , see :: Float
     , sse :: Float
     , ssw :: Float
     , sww :: Float
     , nww :: Float
     , nnw :: Float
     }

empBuck :: ConcBucket
empBuck = CB 0 0 0 0 0 0 0 0


buck2List :: Float -> ConcBucket -> [Float]
buck2List tot (CB a b c d e f g h) = [a / tot,b / tot,c / tot,d / tot,e / tot,f / tot,g / tot,h / tot]


concentration :: Ship -> [Bullet] -> [Float]
concentration Ship{..} bs = buck2List (fromIntegral $ length bs) $ foldl' contOne empBuck bs
  where contOne cb@CB{..} Bullet{..}
          | ne && xBigger = cb { nee = nee + comp }
          | ne            = cb { nne = nne + comp }
          | se && xBigger = cb { see = see + comp }
          | se            = cb { sse = sse + comp }
          | sw && xBigger = cb { sww = sww + comp }
          | sw            = cb { ssw = ssw + comp }
          | nw && xBigger = cb { nww = nww + comp }
          | otherwise     = cb { nnw = nnw + comp }
          where xDiff = bullX - shipX
                yDiff = bullY - shipY
                xBigger = abs xDiff > abs yDiff
                xPos = yDiff >= 0
                yPos = xDiff >= 0
                dist = sqrt (xDiff**2 + yDiff**2) - shipR - bullR
                comp = 1 / (1 + dist)
                ne = xPos && yPos
                se = xPos && not yPos
                sw = not xPos && not yPos
                nw = not xPos && yPos


makeInputs :: CaveGame a -> [Double]
makeInputs CaveGame{..} = (map float2Double $ shipX : shipY : concentration gameShip gameBBullets)
  where Ship{..} = gameShip


makeCont :: [Double] -> Control
makeCont (up:down:left:right:z:_) = Control{..}
  where upness = if up > 0.9 then 1 else 0
        downness = if down > 0.9 then 1 else 0
        leftness = if left > 0.9 then 1 else 0
        rightness = if right > 0.9 then 1 else 0
        cX = rightness - leftness
        cY = upness - downness
        cShoot = z > 0.9


netCont :: Controller Network
netCont = Controller go
  where go net game = (net', makeCont $ getOutput net')
          where net' = stepNetwork net (makeInputs game)


gameScorer :: Genome -> Score
gameScorer g = stepGameN 1000 game
  where net = mkPhenotype g
        game = initGame net netCont


stepGameN :: Int -> CaveGame a -> Score
stepGameN n cg = either id gameScore $ infGames !! n
  where infGames = iterate (simGloss 0.02) (Right cg)


toFitness :: Score -> Double
toFitness Score{..} = fromIntegral scoreScore


gameFit :: GenScorer Score
gameFit = GS gameScorer toFitness (const False)


simGen :: Int -> Genome -> IO ()
simGen n g = simulate disp white 50 (Right $ initGame (mkPhenotype g) netCont) drawGloss (const simGloss)
  where disp = InWindow ("Champion of gen " ++ show n) (scrInt,scrInt) (0,0)
