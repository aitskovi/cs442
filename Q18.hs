import Data.List
import Debug.Trace

data DFA a = DFA [(Int, a, Int)] Int [Int]
            deriving (Show, Eq, Ord)

dfaGen :: (Ord a) => DFA a -> [[a]]
dfaGen (DFA t s e) = sGen (DFA (sort t) s e) [(s, [])]

sGen :: (Ord a) => DFA a -> [(Int, [a])] -> [[a]]
sGen _ [] = []
sGen d as
    | terminated d as   = (pick d as) ++ sGen d (next d as)
    | otherwise         = sGen d (next d as)

-- Applies one transition to all the states we currently have
next :: (Ord a) => DFA a -> [(Int, [a])] -> [(Int, [a])]
next d [] = []
next d as = concatMap (\(s, string) -> (map (\(x,y) -> (x, string ++ [y])) (transitions d s))) as

transitions :: (Ord a) => DFA a -> Int -> [(Int, a)]
transitions (DFA t s e) state = (map (\(_, char, end) -> (end, char)) ts)
    where
        ts = (filter (\(start, _, _) -> (state == start)) t)

terminated :: (Ord a) => DFA a -> [(Int, [a])] -> Bool
terminated (DFA t s e) as = any (\(x, _) -> (any (==x) e)) as

pick :: (Ord a) => DFA a -> [(Int, [a])] -> [[a]]
pick _ [] = []
pick (DFA t s e) as = map (\(_, string) -> string) terminated
    where
        terminated = filter (\(x, _) -> (any (==x) e)) as
