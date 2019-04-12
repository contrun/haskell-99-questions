module Main where

import System.Environment
import Lib
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  args <- fmap (Set.fromList . (map read)) getArgs :: IO (Set Int)
  let questions = map wrap $ filter (\x -> Set.member (fst x) args) $ zip [1..] allQuestions
  sequence_ questions

wrap :: (Int, IO ()) -> IO()
wrap x = do
  putStrLn $ "Running question " ++ (show $ fst x)
  snd x