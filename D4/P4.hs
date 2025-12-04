{-# OPTIONS_GHC -Wno-tabs #-}
module P4 where
import qualified Data.Map as M

type Grid = M.Map (Int, Int) Int
type Enummed = ((Int, Int), Char)

enumerate2D :: [String] -> [Enummed]
enumerate2D x = [
		((i, j), c)
		| (i, row) <- zip [0..] x
		, (j, c) <- zip [0..] row
	]

incrementalInsert :: (Int, Int) -> Grid -> Grid
incrementalInsert (i, j) m
	| Just v <- M.lookup (i, j) m = M.insert (i, j) (v + 1) m
	| otherwise = M.insert (i, j) 1 m

decrementalInsert :: (Int, Int) -> Grid -> Grid
decrementalInsert (i, j) m
	| Just v <- M.lookup (i, j) m = M.insert (i, j) (v - 1) m
	| otherwise = m

makeGrid :: [Enummed] -> Grid -> Grid
makeGrid [] m = m
makeGrid ( ((i, j), c):xs ) m
	| c == '.' = makeGrid xs m
	| c == '@' = makeGrid xs m'
	where
		k = [(i - 1, j - 1), (i - 1, j), (i - 1, j + 1), (i, j - 1), (i, j + 1), (i + 1,j - 1), (i + 1, j), (i + 1, j + 1)]
		mi = case M.lookup (i, j) m of
				Just v -> M.insert (i, j) v m
				otherwise -> M.insert (i, j) 0 m
		m' = foldr (\k acc -> incrementalInsert k acc) mi k

solve :: [Enummed] -> Grid -> Int
solve [] _ = 0
solve (((i, j), c):xs) m
	| c == '@' = if times < 4 then 1 + solve xs m else solve xs m
	| c == '.' = solve xs m
	where
		times = case M.lookup (i, j) m of
							Just v -> v
							Nothing -> 0

-- confused as to what this is? me too buddy
solve' :: [Enummed] -> Grid -> (Int, Grid) -> (Int, Grid)
solve' [] _ c = c
solve' (((i, j), c):xs) m (cnt, mo)
	| c == '@' = if times < 4 then solve' xs m (cnt + 1, m') else solve' xs m (cnt, mo)
	| c == '.' = solve' xs m (cnt, mo)
	where
		times = case M.lookup (i, j) m of
							Just v -> v
							Nothing -> 0
		k = [(i - 1, j - 1), (i - 1, j), (i - 1, j + 1), (i, j - 1), (i, j + 1), (i + 1,j - 1), (i + 1, j), (i + 1, j + 1)]
		mi = case M.lookup (i, j) mo of
				Just v -> M.insert (i, j) (if times < 4 then maxBound :: Int else v) mo
				otherwise -> mo
		m' = foldr (\k acc -> decrementalInsert k acc) mi k

inf_data_struct :: [Enummed] -> (Int, Grid) -> [(Int, Grid)]
inf_data_struct i (c, m) = (co, mo) : inf_data_struct i (co, mo)
			where
				(co, mo) = solve' i m (c, m)

eyeball_a_solution :: [(Int, Grid)] -> Int
eyeball_a_solution (x:y:xs)
	| fst x == fst y = fst x
	| otherwise = eyeball_a_solution (y:xs)

main = do
	i <- fmap (enumerate2D . words) $ readFile "input.txt"
	m <- pure $ makeGrid i M.empty
	-- print $ i
	-- print $ m
	print $ solve i m
	-- DO NOT ACTUALLY DO THIS, THIS IS JUST ME EYEBALLING A SOLUTION
	print $ eyeball_a_solution . inf_data_struct i $ (0, m)
