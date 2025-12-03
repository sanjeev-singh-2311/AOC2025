{-# OPTIONS_GHC -Wno-tabs #-}
module P3 where
import qualified Data.Map as M

type Memo = M.Map (String, Int) Int

solve :: Int -> Memo -> String -> (Int, Memo)
solve _ m xs@[x] = (read xs, m)
solve 1 m xs@(x:xr)
	| Just v <- M.lookup (xs, 1) m = (v, m)
	| otherwise = (p, M.insert (xs, 1) p m')
	where
		a = read [x]
		(b, m') = solve 1 m xr
		p = max a b

solve n m xs@(x:xr)
	| Just v <- M.lookup (xs, n) m = (v, m)
	| n > length xs = (minBound :: Int, m)
	| otherwise = (p, M.insert (xs, n) p m'')
	where
		x' = read [x]
		(a, m') = solve (n - 1) m xr
		(b, m'') = solve n m' xr
		p = max (x' * 10 ^ (n - 1) + a) b

main = do
	i <- fmap words $ readFile "input.txt"
	f <- pure (\x acc -> acc + fst x)
	-- print $ i
	print $ foldr f 0 $ map (solve 2 M.empty) i
	print $ foldr f 0 $ map (solve 12 M.empty) i
