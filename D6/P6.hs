{-# OPTIONS_GHC -Wno-tabs #-}
module P6 where
import qualified Data.Text as T
import Data.List (transpose, unsnoc, groupBy)

solve :: [Integer] -> [T.Text] -> Integer
solve arr [op]
	| op == T.pack "+" = foldr1 (+) arr
	| op == T.pack "*" = foldr1 (*) arr
solve arr (x:xs) = let p = read (T.unpack x) :: Integer in solve (p:arr) xs

solve' :: [String] -> [Integer] -> Integer
solve' [] arr = 0
solve' (x:xs) arr
	| and (map (==' ') x) = solve' xs arr
	| last x == '+' = let p = read (init x) :: Integer in foldr1 (+) (p:arr) + (solve' xs [])
	| last x == '*' = let p = read (init x) :: Integer in foldr1 (*) (p:arr) + (solve' xs [])
	| otherwise = let p = read x :: Integer in solve' xs (p:arr)

main = do
	newlineChar <- pure $ T.pack "\n"
	spaceChar <- pure $ T.pack " "
	emptyChar <- pure $ T.pack ""
	zeroChar <- pure $ T.pack "0"
	raw <- readFile "input.txt"
	sheet1 <- fmap ((transpose . filter (/=[]) . 
								map (filter (/= emptyChar))) . 
								map (T.splitOn spaceChar) . 
								(T.splitOn newlineChar) . T.pack) $ pure raw
	
	print $ foldr1 (+) $ map (solve []) sheet1
	Just (tmp, l) <- pure $ unsnoc $ map (T.unpack) (T.splitOn newlineChar (T.pack raw))
	print $ solve' (reverse . transpose $ tmp) []
