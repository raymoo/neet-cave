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
  putStrLn "Press Enter to continue."
  _ <- getLine
  seed <- randomIO
  let pop =
        newPop seed (PS 150 10 3 defParams { recurrencies = True } smallParams { recurrencies = True } (Just numCons))
  pop' <- caveLoop num pop
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


caveLoop :: Int -> Population -> IO Population
caveLoop n pop
  | n <= 0 = return pop
  | otherwise = do
      printInfo pop
      let (pop',_) = trainOnce gameFit pop
      caveLoop (n - 1) pop'



printInfo :: Population -> IO ()
printInfo pop = do
  putStrLn $ "Generation " ++ show (popGen pop)
  putStrLn $ "Species: " ++ show (speciesCount pop)
  putStrLn $ "High Score: " ++ show (popBScore pop)
  putStrLn ""
