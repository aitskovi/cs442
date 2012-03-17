import Data.List
import Debug.Trace

data DFA a = DFA [(Int, a, Int)] Int [Int]
            deriving (Show, Eq, Ord)

dfaGen :: (Ord a) => DFA a -> [[a]]
dfaGen (DFA t s e) = sGen (DFA (sort t) s e) s

sGen :: (Ord a) => DFA a -> Int -> [[a]]
sGen (DFA t s e) state = if end then []:strings else strings 
    where
        end = any (== state) e
        transitions = map (\(_, char, end) -> (map (char:) (sGen (DFA t s e) end))) 
                            (filter (\(start, _, _) -> state == start) t)
        strings = merge transitions

merge :: (Ord a) => [[[a]]] -> [[a]]
merge [] = []
merge as = (a:(merge (remove a as)))
    where a = pick as

pick :: (Ord a) => [[[a]]] -> [a]
pick [] = []
pick ([]:[]) = []
pick (a:[]) = (head a)
pick (a:as) = 
    if length b <= 1 then b
    else if length (head a) <= length b then (head a) else b
    where
        b = pick as

remove :: (Ord a) => [a] -> [[[a]]] -> [[[a]]]
remove a as = (filter (/= []) (map (filter (/= a)) as))
