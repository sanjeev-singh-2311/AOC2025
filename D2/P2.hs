{-# OPTIONS_GHC -Wno-tabs #-}
module P2 where
import Data.Text (splitOn, pack, strip, Text, unpack, chunksOf)

numberLength :: Int -> Int
numberLength n
	| n == 0 = 1
	| n < 10 = 1
	| otherwise = 1 + numberLength (n `div` 10)

toIntervals :: Text -> (Int, Int)
toIntervals s = (p, q)
	where
		p:q:[] = map (read . unpack) $ splitOn (pack "-") s

isWW :: Int -> Bool
isWW n
	| p `mod` 2 == 1 = False
	| p `mod` 2 == 0 = n `mod` p' == n `div` p'
	where
		p = numberLength n
		p' = 10 ^ (p `div` 2)

isWW' :: Int -> Int -> Bool
isWW' l n
	| l == length ns = False
	| otherwise = (all (== ncc) nc) || isWW' (l + 1) n
	where
		ns@(nn:n') = show n
		ncc:nc = chunksOf l (pack ns)

findWWsum :: (Int, Int) -> Int
findWWsum (l, r) = foldr (+) 0 $ filter isWW [l..r]
-- findWWsum :: (Int, Int) -> [Int]
-- findWWsum (l, r) = filter isWW [l..r]

findWWsum' :: (Int, Int) -> Int
findWWsum' (l, r) = foldr (+) 0 $ filter (isWW' 1) [l..r]
-- findWWsum' :: (Int, Int) -> [Int]
-- findWWsum' (l, r) = filter (isWW' 1) [l..r]


main = do
	i <- fmap ( map (toIntervals . strip) . splitOn (pack ",") . pack ) $ readFile "input.txt"
	print $ foldr (+) 0 $ map findWWsum i
	print $ foldr (+) 0 $ map findWWsum' i
	-- print $ map findWWsum' i
	-- print $ map findWWsum i
