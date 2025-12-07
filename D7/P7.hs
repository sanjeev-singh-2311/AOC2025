{-# OPTIONS_GHC -Wno-tabs #-}
module P7 where
import qualified Data.Set as S
import qualified Data.Map as M

type Coords = (Int, Int)

solve :: [String] -> (Int, Int) -> Coords -> S.Set Coords -> (Integer, S.Set Coords)
solve xs (m, n) (i, j) s
	| or [i < 0, i >= m, j < 0, j >= n] = (0, s)
	| S.member (i, j) s = (0, s)
	| (xs !! i !! j) == 'S' = solve xs (m, n) (i + 1, j) s'
	| (xs !! i !! j) == '.' = solve xs (m, n) (i + 1, j) s'
	| (xs !! i !! j) == '^' = (1 + a + b, so)
	where
		s' = S.insert (i, j) s
		(a, s'') = solve xs (m, n) (i, j - 1) s'
		(b, so) = solve xs (m, n) (i, j + 1) s''

solve' :: [String] -> (Int, Int) -> Coords -> M.Map Coords Integer -> (Integer, M.Map Coords Integer)
solve' xs (m, n) (i, j) mp
	| or [i < 0, i >= m, j < 0, j >= n] = (0, mp)
	| Just v <- M.lookup (i, j) mp = (v, mp)
	| curr == '.' = (dotRightV + dotLeftV + dotUpV, M.insert (i, j) (dotRightV + dotLeftV + dotUpV) dotUpM)
	| curr == '^' = (splitUpV, M.insert (i, j) (splitUpV) splitUpM)
	where
		curr = xs !! i !! j
		(dotRightV, dotRightM) = if and [ j + 1 < n, xs !! i !! (j + 1) == '^' ] then (solve' xs (m, n) (i, j + 1) mp) else (0, mp)

		(dotLeftV, dotLeftM) = if and [ j - 1 >= 0, xs !! i !! (j - 1) == '^' ] then (solve' xs (m, n) (i, j - 1) dotRightM) else (0, dotRightM)

		(dotUpV, dotUpM) = if and [ i - 1 >= 0, let p = xs !! (i - 1) !! j in or [p == '.', p == 'S'] ]
																				then (solve' xs (m, n) (i - 1, j) dotLeftM) else (0, dotLeftM)
		(splitUpV, splitUpM) = if and [ i - 1 >= 0, let p = xs !! (i - 1) !! j in or [p == '.', p == 'S'] ] then (solve' xs (m, n) (i - 1, j) mp) else (0, mp)

main = do
	inp <- fmap (words) $ readFile "input.txt"
	(m, n) <- pure $ (length inp, length (inp !! 0))
	ystart <- pure $ length $ takeWhile (/= 'S') (head inp)
	print $ fst $ solve inp (m, n) (0, ystart) S.empty

	print $ foldr (\(a, b) c -> b + c) 0 $ 
				filter (\((a, b), c) -> a == (m - 1)) $ 
				M.toList $ snd $ 
				foldr (\c (v, acc) -> solve' inp (m, n) c acc) (0, M.singleton (0, ystart) 1) [(i, j) | i <- [0..(m - 1)], j <- [0..(n - 1)]]

