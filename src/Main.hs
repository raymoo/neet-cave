module Main where


import Neet.Examples.CAVE
import Neet
import System.Random
import Data.Serialize
import qualified Data.ByteString as BS

main :: IO ()
main = do
  putStrLn "Play (1), Train (2), or Play with existing AI (3)?"
  ans <- getLine
  case ans of
   "1" -> cavePlay
   "2" -> caveTrain
   "3" -> do
     filecont <- BS.readFile "aigenome"
     case decode filecont of
      Left str -> putStrLn $ "error: " ++ str
      Right org -> simGen (-1) org
   _   -> main


caveTrain :: IO ()
caveTrain = do
  putStrLn "How many generations to train?"
  num <- fmap read getLine
  putStrLn "How many max initial connections?"
  numCons <- fmap read getLine
  putStrLn "How many steps to simulate? (50 steps = 1 seconds)"
  ticks <- fmap read getLine
  putStrLn "Press Enter to continue."
  _ <- getLine
  seed <- randomIO
  let mut = defMutParams { recurrencies = True, delConnChance = 0.3, delNodeChance = 0.03 }
      mutS = defMutParamsS { recurrencies = True, delConnChance = 0.03 }
      dp = defDistParams { delta_t = 5 }
      sp = Target dp (SpeciesTarget (15,25) 0.1)
      params = defParams { specParams = sp, mutParams = mut, mutParamsS = mutS }
      pp = PhaseParams { phaseAddAmount = 10, phaseWaitTime = 5 }
      pop =
        newPop seed (PS 150 10 3 params (Just numCons) (Just pp))
  pop' <- caveLoop ticks num pop
  printInfo pop'
  putStrLn "Push Enter to view genome."
  _ <- getLine
  renderGenome (popBOrg pop')
  let simOrg = do
        putStrLn "Push Enter to watch the best AI play."
        _ <- getLine
        simGen (popGen pop') (popBOrg pop')
  putStrLn "Save AI to file? (y)es, or any other input to not save."
  choice <- getLine
  case choice of
   "y" -> do
     let encoded = encode (popBOrg pop')
     BS.writeFile "aigenome" encoded
     putStrLn "File output to ./aigenome"
     simOrg
   _ -> simOrg


-- | Two args are number of ticks and number of generations.
caveLoop :: Int -> Int -> Population -> IO Population
caveLoop ticks n pop
  | n <= 0 = return pop
  | otherwise = do
      printInfo pop
      let (pop',_) = trainOnce (gameFit ticks) pop
      caveLoop ticks (n - 1) pop'



printInfo :: Population -> IO ()
printInfo pop = do
  putStrLn $ "Generation " ++ show (popGen pop)
  putStrLn $ "Species: " ++ show (speciesCount pop)
  putStrLn $ "Species Generated: " ++ (show . nextSpec $ pop)
  putStrLn $ "High Score: " ++ show (popBScore pop)
  putStrLn ""

