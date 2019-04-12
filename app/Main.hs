module Main where

import System.Environment
import Lib
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  args <- fmap (Set.fromList . (map read)) getArgs :: IO (Set Int)
  let questions = map wrapper $ filter (\x -> Set.member (fst x) args) $ zip [1..] all_questions
  sequence_ questions

wrapper :: (Int, IO ()) -> IO()
wrapper x = do
  putStrLn $ "Running question " ++ (show $ fst x)
  snd x