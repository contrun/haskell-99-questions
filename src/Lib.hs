{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Lib
    (
    allQuestions,
    ) where

import Data.Sort (sort, sortBy)
import Data.List (group)
import Data.Ord (compare)
import System.Random (newStdGen, randomIO, randomR)
import Control.Monad
import Control.Monad.Loops (whileM)
import qualified Data.HashMap.Lazy as M

failIf :: Bool -> String -> IO ()
failIf b s = if b then fail s else return ()

failIfM :: IO Bool -> String -> IO()
failIfM b s =  b >>= flip failIf s

allQuestions = [question01, question02, question03, question04, question05, question06, question07, question08, question09, question10, question11, question12, question13, question14, question15, question16, question17, question18, question19, question20, question21, question22, question23, question24, question25, question26, question27, question28, question29, question30, question31, question32, question33, question34, question35, question36, question37, question38, question39, question40, question41, question42, question43, question44, question45, question46, question47, question48, question49, question50, question51, question52, question53, question54, question55, question56, question57, question58, question59, question60, question61, question62, question63, question64, question65, question66, question67, question68, question69, question70, question71, question72, question73, question74, question75, question76, question77, question78, question79, question80, question81, question82, question83, question84, question85, question86, question87, question88, question89, question90, question91, question92, question93, question94, question95, question96, question97, question98, question99]

helper01 :: [a] -> a
helper01 [] = undefined
helper01 [x] = x
helper01 (x:ts) = helper01 ts

question01 :: IO ()
question01 = do
  let myList = [[1, 23, 4], [1], [2, 3, 4]]
  failIf ([4, 1, 4] /= (map helper01 myList)) ""
  -- helper01 []

helper02 :: [a] -> a
helper02 [] = undefined
helper02 [x] = undefined
helper02 [x, y] = x
helper02 (x:ts) = helper02 ts

question02 :: IO ()
question02 = do
  let myList = [[1, 23, 4], [1, 4], [2, 3, 4]]
  failIf ([23, 1, 3] /= (map helper02 myList)) ""

helper03 :: [a] -> Int -> a
helper03 as n = snd $ (filter (\x -> (fst x) == n) $ zip [1..] as) !! 0

question03 :: IO ()
question03 = do
  let tests = [([1, 2, 3], 1), ([1, 2, 3], 2), ([1, 2, 3], 3)]
  failIf  ([1, 2, 3] /=  (map (uncurry $ helper03) tests)) ""

helper04 :: [a] -> Int
helper04 [] = 0
helper04 (h:t) = 1 + helper04 t

question04 :: IO ()
question04 = do
  let tests = [[], [1], [1, 2], [1, 2, 3]]
  failIf  ([0, 1, 2, 3] /=  (map helper04 tests)) ""

helper05 :: [a] -> [a]
helper05 [] = []
helper05 (h:t) = helper05 t ++ [h]

question05 :: IO ()
question05 = do
  let tests = [[], [1], [1, 2], [1, 2, 3]]
  failIf  ([[], [1], [2, 1], [3, 2, 1]] /=  (map helper05 tests)) ""

helper06 :: Eq a => [a] -> Bool
helper06 x = (helper05 x) == x

question06 :: IO ()
question06 = do
  let tests = [[], [1], [1, 2], [1, 2, 3], [1, 2, 1]]
  failIf ([True, True, False, False, True] /=  (map helper06 tests)) ""

data NestedList a = Elem a | List [NestedList a]
helper07 :: NestedList a -> [a]
helper07 (Elem x) = [x]
helper07 (List as) = foldl (\x y -> x ++ (helper07 y)) [] as

question07 :: IO ()
question07 = do
  let test = (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
  failIf (helper07 test /= [1, 2, 3, 4, 5]) ""

helper08 :: Eq a => [a] -> [a]
helper08 [] = []
helper08 (h:t) = realHelper08 h t
  where realHelper08 h as = case as of {
    []       -> [h];
    (h1: t1) -> if h1 == h then (realHelper08 h t1) else (h:realHelper08 h1 t1);
  }

question08 :: IO ()
question08 = do
  failIf (helper08 "aaaabccaadeeee" /= "abcade") ""

helper09 :: Eq a => [a] -> [[a]]
helper09 [] = []
helper09 (h:t) = realHelper09 [] [h] t
  where realHelper09 :: Eq a => [[a]] -> [a] -> [a] -> [[a]]
        realHelper09 acc h t = case t of {
          [] -> acc ++ [h];
          (h1: t1) -> if h1 `elem`  h then (realHelper09 acc (h++[h1]) t1) else (realHelper09 (acc++[h]) [h1] t1);
          }

question09 :: IO ()
question09 = do
  failIf (helper09 "aaaabccaadeeee" /= ["aaaa", "b", "cc", "aa", "d", "eeee"]) ""


helper10 :: Eq a => [a] -> [(Int, a)]
helper10 = map (\x -> (length x, head x)) . helper09

question10 :: IO ()
question10 = do
  failIf (helper10 "aaaabccaadeeee"  /= [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')]) ""

data MyIntA a = Multiple Int a | Single a deriving Eq
helper11 :: Eq a => [a] -> [MyIntA a]
helper11 = map (\x -> if length x == 1 then Single (head x) else Multiple (length x) (head x)) . helper09

question11 :: IO ()
question11 = do
  failIf (helper11 "aaaabccaadeeee"  /= [(Multiple 4 'a'), (Single 'b'), (Multiple 2 'c'), (Multiple 2 'a'), (Single 'd'), (Multiple 4 'e')]) ""

helper12 :: [MyIntA a] -> [a]
helper12 = concat . map (\x -> case x of {
  Multiple n y -> replicate n y;
  Single y -> [y];
})

question12 :: IO ()
question12 = do
  failIf ("aaaabccaadeeee"  /= helper12 [(Multiple 4 'a'), (Single 'b'), (Multiple 2 'c'), (Multiple 2 'a'), (Single 'd'), (Multiple 4 'e')]) ""

-- didn't see the difference
helper13 :: Eq a => [a] -> [MyIntA a]
helper13 = helper11

question13 :: IO ()
question13 = question11

helper14 :: [a] -> [a]
helper14 = concat . map (\x -> [x, x])

question14 :: IO ()
question14 = do
  failIf (helper14 [1, 2, 3] /= [1, 1, 2, 2, 3, 3]) ""

helper15 :: [a] -> Int -> [a]
helper15 l n = concat $ map (f n) l
  where
    f 0 x = []
    f n x = (x: f (n-1) x)

question15 :: IO ()
question15 = do
  failIf (helper15 [1, 2, 3] 2 /= [1, 1, 2, 2, 3, 3]) ""

helper16 :: [a] -> Int -> [a]
helper16 l n = map snd $ filter (\x -> (fst x) `mod` n /= 0) $ zip [1..] l

question16 :: IO ()
question16 = do
  failIf (helper16 "abcdefg" 3 /=  "abdeg") ""

helper17 :: [a] -> Int -> ([a], [a])
helper17 as n = foldl go ([], []) $ zip [n, (n-1)..] as
  where
    go (x, y) (i, v)
      | i <= 0 = (x, y++[v])
      | otherwise = (x++[v], y)

question17 :: IO ()
question17 = do
  failIf ((helper17 "abcdefg" 3) /= ("abc", "defg")) ""

helper18 :: [a] -> Int -> Int -> [a]
helper18 as m n = map snd $ filter (\(x, _) -> x >= m && x <= n) $ zip [1..] as

question18 :: IO ()
question18 = do
  failIf ((helper18 "abcdefg" 3 5) /= ("cde")) ""

helper19 :: [a] -> Int -> [a]
helper19 as n = uncurry (flip (++)) $ foldl go ([], []) $ zip [n `rem` (length as), ((n `rem` length as)-1)..] as
                                             where
                                               go (x, y) (i, v)
                                                 | i <= 0 = (x, y++[v])
                                                 | otherwise = (x++[v], y)

question19 :: IO ()
question19 = do
  failIf ((helper19 "abcdefg" 12) /= "fgabcde") ""


helper20 :: [a] -> Int -> [a]
helper20 as n = map snd $ filter (\(i, _) -> i /= n) $ zip [1..] as

question20 :: IO ()
question20 = do
  failIf ("abcdefg" /= helper20 "abcdhefg" 5) ""

helper21 :: [a] -> Int -> a -> [a]
helper21 as n x = foldl go [] $ zip [1..] as
  where go l (c, v)
          | c == n = l++[x, v]
          | otherwise = l ++ [v]

question21 :: IO ()
question21 = do
  failIf ("abcde" /=  helper21 "abde" 3 'c') ""

helper22 :: Int -> Int -> [Int]
helper22 m n
  | m == n = [m]
  | m < n = (m: helper22 (m+1) n)
  | m > n = (m: helper22 (m-1) n)

question22 :: IO ()
question22 = do
  failIf (helper22 1 2 /=  [1, 2]) ""
  failIf (helper22 2 1 /=  [2, 1]) ""
  failIf (helper22 2 2 /=  [2]) ""

helper23 :: [a] -> Int -> IO [a]
helper23 as n = let newRand = randomIO :: IO Int in
  replicateM n $ newRand >>= \x -> return $ as !! (mod x $ length as)

question23 :: IO ()
question23 = do
  let l = [1, 23, 435, 345, 6, 456, 234]
  helper23 l 5 >>= \x -> failIf (not $ or [y `elem` l | y <- x]) ""

helper24 :: Int -> Int -> IO [Int]
helper24 m n = newStdGen >>= \x -> return (go x [])
  where
    go g acc
      | (length acc) == n = acc
      | otherwise = let (a, g') = randomR (1, m) g in if a `elem` acc then go g' acc else go g' (a:acc)

question24 :: IO ()
question24 = do
  helper24 10 5 >>= print

helper25 :: [a] -> IO [a]
helper25 as = (\ls -> map (\x -> as !! (x-1)) ls) <$> (helper24 (length as) (length as))

question25 :: IO ()
question25 = do
  let l = [1, 2, 3, 4, 5]
  helper25 l >>= \x -> failIf (sort l /= sort x) ""

helper26 :: [a] -> [(a, a, a)]
helper26 as = [(x, y, z) | (i, x) <- zip [1..] as, (j, y) <- zip [1..] as, (k, z) <- zip [1..] as, i < j && j < k]

question26 :: IO ()
question26 = do
  failIf (sort (helper26 [1, 2, 3, 4]) /= sort [(1,2,3), (1,2,4), (1,3,4), (2,3,4)]) ""

helper27 :: Int -> Int -> Int -> Int
helper27 a b c = product [1..(a+b+c)] `div` (product [1..a]) `div` (product [1..b]) `div` (product [1..c])

question27 :: IO ()
question27 = do
  failIf (helper27 2 3 4 /= 1260) ""

helper28A :: [[a]] -> [[a]]
helper28A as = sortBy (\l1 l2 -> compare (length l1) (length l2)) as

helper28B :: [[a]] -> [[a]]
helper28B as = let m = M.fromList $ map (\x -> (head x, length x)) $ group $ sort [(length x) | x <- as] in
   sortBy (\l1 l2 -> compare (m M.! (length l1)) (m M.! (length l2))) as

question28 :: IO ()
question28 = do
  failIf ((helper28A ["sdf", "s", "sx"]) /= ["s", "sx", "sdf"]) ""
  let l = [[1], [1, 2], [1,3], [1, 2, 3], [3]]
  failIf ((helper28B l) /= [[1,2,3],[1],[1,2],[1,3],[3]]) ""

--helper29 ::

question29 :: IO ()
question29 = undefined

--helper30 ::

question30 :: IO ()
question30 = undefined

--helper31 ::

question31 :: IO ()
question31 = undefined

--helper32 ::

question32 :: IO ()
question32 = undefined

--helper33 ::

question33 :: IO ()
question33 = undefined

--helper34 ::

question34 :: IO ()
question34 = undefined

--helper35 ::

question35 :: IO ()
question35 = undefined

--helper36 ::

question36 :: IO ()
question36 = undefined

--helper37 ::

question37 :: IO ()
question37 = undefined

--helper38 ::

question38 :: IO ()
question38 = undefined

--helper39 ::

question39 :: IO ()
question39 = undefined

--helper40 ::

question40 :: IO ()
question40 = undefined

--helper41 ::

question41 :: IO ()
question41 = undefined

--helper42 ::

question42 :: IO ()
question42 = undefined

--helper43 ::

question43 :: IO ()
question43 = undefined

--helper44 ::

question44 :: IO ()
question44 = undefined

--helper45 ::

question45 :: IO ()
question45 = undefined

--helper46 ::

question46 :: IO ()
question46 = undefined

--helper47 ::

question47 :: IO ()
question47 = undefined

--helper48 ::

question48 :: IO ()
question48 = undefined

--helper49 ::

question49 :: IO ()
question49 = undefined

--helper50 ::

question50 :: IO ()
question50 = undefined

--helper51 ::

question51 :: IO ()
question51 = undefined

--helper52 ::

question52 :: IO ()
question52 = undefined

--helper53 ::

question53 :: IO ()
question53 = undefined

--helper54 ::

question54 :: IO ()
question54 = undefined

--helper55 ::

question55 :: IO ()
question55 = undefined

--helper56 ::

question56 :: IO ()
question56 = undefined

--helper57 ::

question57 :: IO ()
question57 = undefined

--helper58 ::

question58 :: IO ()
question58 = undefined

--helper59 ::

question59 :: IO ()
question59 = undefined

--helper60 ::

question60 :: IO ()
question60 = undefined

--helper61 ::

question61 :: IO ()
question61 = undefined

--helper62 ::

question62 :: IO ()
question62 = undefined

--helper63 ::

question63 :: IO ()
question63 = undefined

--helper64 ::

question64 :: IO ()
question64 = undefined

--helper65 ::

question65 :: IO ()
question65 = undefined

--helper66 ::

question66 :: IO ()
question66 = undefined

--helper67 ::

question67 :: IO ()
question67 = undefined

--helper68 ::

question68 :: IO ()
question68 = undefined

--helper69 ::

question69 :: IO ()
question69 = undefined

--helper70 ::

question70 :: IO ()
question70 = undefined

--helper71 ::

question71 :: IO ()
question71 = undefined

--helper72 ::

question72 :: IO ()
question72 = undefined

--helper73 ::

question73 :: IO ()
question73 = undefined

--helper74 ::

question74 :: IO ()
question74 = undefined

--helper75 ::

question75 :: IO ()
question75 = undefined

--helper76 ::

question76 :: IO ()
question76 = undefined

--helper77 ::

question77 :: IO ()
question77 = undefined

--helper78 ::

question78 :: IO ()
question78 = undefined

--helper79 ::

question79 :: IO ()
question79 = undefined

--helper80 ::

question80 :: IO ()
question80 = undefined

--helper81 ::

question81 :: IO ()
question81 = undefined

--helper82 ::

question82 :: IO ()
question82 = undefined

--helper83 ::

question83 :: IO ()
question83 = undefined

--helper84 ::

question84 :: IO ()
question84 = undefined

--helper85 ::

question85 :: IO ()
question85 = undefined

--helper86 ::

question86 :: IO ()
question86 = undefined

--helper87 ::

question87 :: IO ()
question87 = undefined

--helper88 ::

question88 :: IO ()
question88 = undefined

--helper89 ::

question89 :: IO ()
question89 = undefined

--helper90 ::

question90 :: IO ()
question90 = undefined

--helper91 ::

question91 :: IO ()
question91 = undefined

--helper92 ::

question92 :: IO ()
question92 = undefined

--helper93 ::

question93 :: IO ()
question93 = undefined

--helper94 ::

question94 :: IO ()
question94 = undefined

--helper95 ::

question95 :: IO ()
question95 = undefined

--helper96 ::

question96 :: IO ()
question96 = undefined

--helper97 ::

question97 :: IO ()
question97 = undefined

--helper98 ::

question98 :: IO ()
question98 = undefined

--helper99 ::

question99 :: IO ()
question99 = undefined

