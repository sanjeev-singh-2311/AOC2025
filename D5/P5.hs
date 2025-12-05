{-# OPTIONS_GHC -Wno-tabs #-}
module P5 where
import Data.List (groupBy, sortBy)

contains :: Char -> String -> Bool
contains _ "" = False
contains c (x:xs) = x == c || contains c xs

makeIntervals :: [String] -> [(Integer, Integer)]
makeIntervals [] = []
makeIntervals (x:xs) = (n1, n2) : makeIntervals xs
	where
		n1 = read (takeWhile (/= '-') x) :: Integer
		n2 = read (drop 1 $ dropWhile (/= '-') x) :: Integer

mergeIntervals :: [(Integer, Integer)] -> [(Integer, Integer)]
mergeIntervals [] = []
mergeIntervals [x] = [x]
mergeIntervals ((a,b):(c,d):xs)
	| c <= b && b <= d = mergeIntervals ((a,d):xs)
	| b >= d = mergeIntervals ((a, b):xs)
	| otherwise = (a, b) : mergeIntervals ((c,d):xs)

solve :: [(Integer, Integer)] -> String -> Integer
solve [] _ = 0
solve ((a, b):xs) s
	| a <= s' && s' <= b = 1
	| otherwise = solve xs s
	where
		s' = read s :: Integer

main = do
	inter:xs <- fmap (groupBy (\x y -> contains '-' x && (contains '-' y)) . words) $ readFile "input.txt"
	vals <- pure $ concat xs
	print $ foldr (+) 0 $ map (solve $ makeIntervals inter) vals
	print $ foldr (+) 0 $ map (\(a,b) -> b - a + 1) $ mergeIntervals $ sortBy (\(a,b) (c,d) -> compare a c) $ makeIntervals inter
