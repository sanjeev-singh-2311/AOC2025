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
solve' xs (m, n) (i, j) mp = undefined -- Todo : Did manage to do in Python but need to figure out how to do a BFS here


main = do
	inp <- fmap (words) $ readFile "input.txt"
	ystart <- pure $ length $ takeWhile (/= 'S') (head inp)
	print $ fst $ solve inp (length inp, length (inp !! 0)) (0, ystart) S.empty
	-- mp <- pure  $ solve' inp (length inp, length (inp !! 0)) (0, ystart) $ M.singleton (0, ystart) 1
	-- print $ fst mp
	-- print $ filter (\((a, b), c) -> a == (length inp - 1)) $ M.toList (snd mp)
