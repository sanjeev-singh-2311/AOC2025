{-# OPTIONS_GHC -Wno-tabs #-}
module P1 where

solve :: [String] -> Int -> Int -> Int
solve [] c _ = c
solve ((d:mov):xs) c pos
	| d'' == 0 = solve xs (c + 1) d''
	| otherwise = solve xs c d''
	where
		m = read mov :: Int
		d' = if d == 'L' then pos - m else pos + m
		d'' = (d' `mod` 100 + 100) `mod` 100

solve' :: [String] -> Int -> Int -> Int
solve' [] c _ = c
solve' ((d:mov):xs) c pos
	| d == 'L' && pos > 0 && d' <= 0 = solve' xs (c' + 1) d''
	| d == 'R' && pos < 100 && d' >= 100 = solve' xs (c' + 1) d''
	| otherwise = solve' xs c' d''
	where
		m = read mov :: Int
		c' = c + m `div` 100
		m' = m `mod` 100
		d' = if d == 'L' then pos - m' else pos + m'
		d'' = (d' `mod` 100 + 100) `mod` 100

main = do
	i <- readFile "input.txt"
	print $ solve (words i) 0 50
	print $ solve' (words i) 0 50
