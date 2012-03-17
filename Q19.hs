import Data.List
import Debug.Trace

data NFA a = NFA [(Int, a, [Int])] Int [Int]
            deriving (Show, Eq, Ord)

nfaGen :: (Ord a) => NFA a -> [[a]]
nfaGen (NFA t s e) = sGen (NFA (sort t) s e) [(s, [])]

sGen :: (Ord a) => NFA a -> [(Int, [a])] -> [[a]]
sGen _ [] = []
sGen d as
    | terminated d as   = (pick d as) ++ sGen d (next d as)
    | otherwise         = sGen d (next d as)

-- Applies one transition to all the states we currently have
next :: (Ord a) => NFA a -> [(Int, [a])] -> [(Int, [a])]
next d [] = []
next d as = concatMap (\(s, string) -> (map (\(x,y) -> (x, string ++ [y])) (transitions d s))) as

transitions :: (Ord a) => NFA a -> Int -> [(Int, a)]
transitions (NFA t s e) state = (concatMap allStates ts)
    where
        allStates = \(_, char, end) -> (map (\ending -> (ending, char)) end)
        ts = (filter (\(start, _, _) -> (state == start)) t)

terminated :: (Ord a) => NFA a -> [(Int, [a])] -> Bool
terminated (NFA t s e) as = any (\(x, _) -> (any (==x) e)) as

pick :: (Ord a) => NFA a -> [(Int, [a])] -> [[a]]
pick _ [] = []
pick (NFA t s e) as = map (\(_, string) -> string) terminated
    where
        terminated = filter (\(x, _) -> (any (==x) e)) as
