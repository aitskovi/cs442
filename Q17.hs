data RE a =
    Empty
    | Epsilon
    | Symbol a
    | Alt (RE a) (RE a)
    | Concat (RE a) (RE a)
    | Repeat (RE a)
    deriving (Show, Eq, Ord)

reGen :: (Ord a) => RE a -> [[a]]
reGen Empty = []
reGen Epsilon = [[]]
reGen (Symbol a) = [[a]]
reGen (Alt a b) = merge ((reGen a):(reGen b):[])
reGen (Concat a b) = 
    merge (cross (reGen a) (reGen b))        
reGen (Repeat a) = 
    if a == Empty || a == Epsilon
        then [[]]
        else []:(reGen (Concat a (Repeat a)))

-- Merges two ordered lists.
-- merge :: (Ord a) => [[a]] -> [[a]] -> [[a]]
-- merge a [] = a
-- merge [] b = b
-- merge (a:as) (b:bs) = 
    --if a == b then a:(merge as bs)
    --else if length a < length b then a:(merge as (b:bs))
    --else if length b < length a then b:(merge (a:as) bs)
    --else if a < b then a:(merge as (b:bs))
    --else b:(merge (a:as) bs)

merge :: (Ord a) => [[[a]]] -> [[a]]
merge [] = []
merge as = (a:(merge (remove a as)))
    where a = pick as

pick :: (Ord a) => [[[a]]] -> [a]
pick [] = []
pick (a:[]) = (head a)
pick (a1:a2:as) = 
    if length (head a1) < length (head a2) then (head a1)
    else if length (head a2) < length (head a1) then pick (a2:as)
    else if (head a1) < (head a2) then (head a1)
    else pick (a2:as)

remove :: (Ord a) => [a] -> [[[a]]] -> [[[a]]]
remove a as = (filter (/= []) (map (filter (/= a)) as))

cross :: (Ord a) => [[a]] -> [[a]] -> [[[a]]]
cross a [] = [a]
cross [] b = []
cross (a:as) b =  ((map (a ++) b):(cross as b))

-- reGen (Concat (Alt (Concat (Symbol "a") (Symbol "b")) (Symbol "a")) (Alt (Concat (Symbol "b") (Symbol "a")) (Symbol "a")))
