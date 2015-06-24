{-# LANGUAGE RankNTypes #-}
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
module Neet.Examples.CAVE (cavePlay, gameFit, simGen, Pattern(..)) where


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Neet

import Data.List (foldl', sortBy)

import GHC.Float

import Data.Maybe


deg :: Float -> Float
deg d = 2 * pi * d / 360


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
hitShip Ship{..} Bullet{..} = (xDiff*xDiff + yDiff*yDiff) <= totalR*totalR
  where xDiff = bullX - shipX
        yDiff = bullY - shipY
        totalR = bullR + shipR


hitBoss :: Boss a -> Bullet -> Bool
hitBoss Boss{..} Bullet{..} = ((bullX - bossX)**2 + (bullY - bossY)**2) <= (bullR + bossR)**2


data WavePattern =
  WavePattern { wpStartAngle :: Float
              , wpInterval   :: Float
              , wpSpacing    :: Float
              , wpVel        :: Float
              , wpDur        :: Float
              }

newtype Pattern a = Pat { getPattern :: Float -> CaveGame a -> ([Bullet], Maybe (Pattern a))
                        }


-- | Do patterns in sequence
patSeries :: [Pattern a] -> Pattern a
patSeries pats = Pat $ go pats
  where go [] _ _ = ([],Nothing)
        go (Pat f:ps) dt cg = case f dt cg of
          (bs, Just newPat) -> (bs, Just . Pat $ go (newPat:ps))
          (bs, Nothing    ) -> (bs, Just . Pat $ go ps)


-- | Do patterns in parallel
patPara :: [Pattern a] -> Pattern a
patPara = Pat . go
  where go [] _ _ = ([], Nothing)
        go ps dt cg = (bullets, fmap (Pat . go) newPats)
          where res = map (\p -> getPattern p dt cg) ps
                bullets = res >>= fst
                newPats = case mapMaybe snd res of
                  [] -> Nothing
                  ps' -> Just ps'


wavePattern :: WavePattern -> Pattern a
wavePattern WavePattern{..} = Pat $ go wpDur wpInterval wpStartAngle
  where go :: Float -> Float -> Float -> Float -> CaveGame a -> ([Bullet], Maybe (Pattern a))
        go remTime intLeft aim dt CaveGame{ gameBoss = Boss{..}, gameShip = Ship{..} }
          | remTime <= 0 = ([], Nothing)
          | otherwise = (newBulls, Just . Pat $ go (remTime - dt) (newTimer) (aim + dt * wpSpacing))
          where doShoot = intLeft <= 0
                newTimer
                  | doShoot = wpInterval
                  | otherwise = intLeft - dt
                newBulls
                  | doShoot = [Bullet bossX bossY (cos aim * wpVel) (sin aim * wpVel) 0.02]
                  | otherwise = []


funPat :: Pattern a
funPat = patPara [wavePattern wave1, wavePattern wave2]
  where wave1 = WavePattern { wpStartAngle = 0
                            , wpInterval = 0.05
                            , wpSpacing = 2 * pi + 0.1
                            , wpVel = 2
                            , wpDur = 9999999999999
                            }
        wave2 = WavePattern { wpStartAngle = 0
                            , wpInterval = 0.1
                            , wpSpacing = 0.2 - pi
                            , wpVel = 0.5
                            , wpDur = 9999999999999
                            } 



data Boss a =
  Boss { bossX :: Float
       , bossY :: Float
       , bossR :: Float
       , bossP :: Maybe (Pattern a)
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
           , gameBoss :: !(Boss cont)
           , gameSBullets :: [Bullet]
           , gameBBullets :: [Bullet]
           , gameContState :: !cont
           , gameController :: Controller cont
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
           , gameBBullets = newBBulls ++ steppedBBullets
           , gameContState = newState
           , gameController = gameController
           , gameScore = newScore
           } 
  where (newState, cont) = control gameController gameContState cg
        Ship{..} = gameShip
        
        newShip = Ship { shipX = bound (-1, 1) $ shipX + dt * (cX cont) * shipSpeed * spdMult
                       , shipY = bound (-1, 1) $ shipY + dt * (cY cont) * shipSpeed * spdMult
                       , shipR = shipR
                       , shipSpeed = shipSpeed
                       , shipTimer = if shipTimer <= 0 then shootInterval else shipTimer - dt
                       }
        spdMult = if cShoot cont then 0.3 else 1
        Boss{..} = gameBoss

        (newBBulls, newPat) = case bossP of
          Nothing -> ([], Nothing)
          Just pat -> getPattern pat dt cg
        
        newBoss = gameBoss { bossP = newPat }


        outBounds :: Bullet -> Bool
        outBounds Bullet{..} =
          bullX < (-1) - bullR ||
          bullX > 1 + bullR ||
          bullY < (-1) - bullR ||
          bullY > 1 + bullR  

        stepSBullet :: Bullet -> Either Int Bullet
        stepSBullet b@Bullet{..}
          | outBounds b        = Left 0
          | hitBoss gameBoss b = Left $ 9 * round (shipY + 1) + 1
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


drawBoss :: Boss a -> Picture
drawBoss Boss{..} = Color red (Scale screen screen (Translate bossX bossY (ThickCircle bossR bossR)))


drawGame :: CaveGame a -> Picture
drawGame CaveGame{..} = Pictures $ [sBulPic, shipPic, bBulPic, bossPic, scorePic]
  where sBulPic = Pictures $ map (drawBullet blue) gameSBullets
        bBulPic = Pictures $ map (drawBullet green) gameBBullets
        shipPic = drawShip gameShip
        bossPic = drawBoss gameBoss
        scorePic = Translate (-200) 200 (Text (show (scoreScore gameScore)))


drawScore :: Score -> Picture
drawScore scr = Translate (-200) 200 (Text (show (scoreScore scr)))


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
           , gameScore = Score 0 0
           } 
  where initBoss = Boss 0 0.5 0.2 (Just funPat)
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


nearestFour :: Ship -> [Bullet] -> [Float]
nearestFour Ship{..} bs = padded >>= toDubs
  where bullDist Bullet{..} =
          let xDiff = bullX - shipX
              yDiff = bullY - shipY
          in (xDiff, yDiff, xDiff * xDiff + yDiff * yDiff )
        dists = map bullDist bs
        filtdists = filter (\(xd,yd,_) -> xd < 0.5 && yd < 0.5) $ dists
        close = take 4 . sortBy (\(_,_,d1) (_,_,d2) -> compare d1 d2) $ usedDists

        enoughClose = length filtdists >= 4

        usedDists
          | enoughClose = filtdists
          | otherwise = dists

        usedLength = length usedDists

        padded
          | enoughClose || usedLength == 4 = close
          | otherwise = replicate (4 - usedLength) (0, 1,-1) ++ close
        toDubs (xd, yd, _) = [xd,yd]


makeInputs :: CaveGame a -> [Double]
makeInputs CaveGame{..} = (map float2Double $ shipX : shipY : nearestFour gameShip gameBBullets)
  where Ship{..} = gameShip


makeCont :: [Double] -> Control
makeCont (horiz:vert:z:_) = Control{..}
  where upness = if vert > 0.9 then 1 else 0
        downness = if vert < 0.1 then 1 else 0
        leftness = if horiz < 0.1 then 1 else 0
        rightness = if horiz > 0.9 then 1 else 0
        cX = rightness - leftness
        cY = upness - downness
        cShoot = z > 0.9


netCont :: Controller Network
netCont = Controller go
  where go net game = (net', makeCont $ getOutput net')
          where net' = stepNetwork net (makeInputs game)


gameScorer :: Genome -> Score
gameScorer g = stepGameN 2000 game
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
