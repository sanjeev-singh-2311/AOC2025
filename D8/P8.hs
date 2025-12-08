{-# OPTIONS_GHC -Wno-tabs #-}
module P8 where

import qualified Data.Map as M
import qualified Data.List as L

type Coord = (Integer, Integer, Integer)
type DSU = M.Map Int Int

toCoord :: String -> Coord
toCoord x = (a, b, c)
	where
		a = read (takeWhile (/= ',') x) :: Integer
		b = read (takeWhile (/= ',') $ drop 1 $ dropWhile (/=',') x) :: Integer
		c = read (drop 1 $ dropWhile (/= ',') $ drop 1 $ dropWhile (/=',') x) :: Integer

dist :: [Coord] -> (Int, Int) -> Integer
dist xs (a, b) = (i - i') ^ 2 + (j - j') ^ 2 + (k - k') ^ 2
	where
		(i, j, k) = xs !! a
		(i', j', k') = xs !! b

makeSet :: Int -> (DSU, DSU) -> (DSU, DSU)
makeSet v (parent, size) = (M.insert v v parent, M.insert v 1 size)

findSet :: Int -> DSU -> (Int, DSU)
findSet u parent
	| Nothing <- M.lookup u parent = (-1, parent)
	| Just v <- M.lookup u parent = if v == u then (v, parent) else 
																	let (a, p) = findSet v parent in (a, M.insert u a p)

sizeUnion :: (Int, Int) -> (DSU, DSU) -> (DSU, DSU)
sizeUnion (u, v) (parent, size) = if (a == b) then (parent, size) else (M.insert b' a' parent, M.insert a' s' size)
	where
		(a, p) = findSet u parent
		(b, p') = findSet v p
		s1 = case M.lookup a size of
					Just ss -> ss
					Nothing -> 0
		s2 = case M.lookup b size of
					Just ss -> ss
					Nothing -> 0
		(a', b') = if s1 < s2 then (b, a) else (a, b)
		s' = s1 + s2

allEqual :: [(Int, Int)] -> Int -> Int -> (DSU, DSU) -> (DSU, DSU, Int)
allEqual [] n l (parent, size) = (parent, size, l - 1)
allEqual (x:xs) n l (parent, size) = let (p', s') = sizeUnion x (parent, size) in
										if (length (L.group . (map (\i -> fst (findSet i p'))) $ [0..(l - 1)]) == 1) then
											(p', s', n)
										else allEqual xs (n + 1) l (p', s')
			
main = do
	inp <- fmap ((map toCoord) . words) $ readFile "./input.txt"
	n <- pure $ length inp
	m <- pure 1000

	distList <- pure $ L.sort $ map ( \c -> ((dist inp c), c)  ) [(i, j) | i <- [0..(n - 1)], j <- [(i + 1)..(n - 1)]]
	(parent, size) <- pure $ foldr (makeSet) (M.empty, M.empty) [0..(n - 1)]
	(p, s) <- pure $ foldr (sizeUnion) (parent, size) $ map (\(i, j) -> j) $ take m distList
	print $ foldr (\(i, j) acc -> acc * j) 1 $ take 3 $ L.sortBy (\(a, b) (c, d) -> compare d b) $ M.toList . snd $ (p, s)

	(p', s', n) <- pure $ allEqual (map (\(i, j) -> j) (drop m distList)) m n (p, s)
	(_, (i, j)) <- pure $ distList !! n
	(x1, _, _) <- pure $ (inp !! i) 
	(x2, _, _) <- pure $ (inp !! j) 
	print $ x1 * x2
