module SimpleFunctions where

-- a)
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst p (x:xs)
	| p x = x:filterFirst p xs
	| otherwise = xs

-- b)
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p xs  = reverse(filterFirst p (reverse xs))
	


--c
split :: [a] -> ([a],[a])
split []       = ([],[])
split [x]      = ([x],[])
split (x:y:zs) = (x:left, y:right)
  where
    left  = fst (split zs)
    right = snd (split zs)

-- d)
interleave :: ([a],[a]) -> [a]
interleave ([], right) = right
interleave (left, [])  = left
interleave (x:xs,y:ys) = x:y:interleave (xs,ys)

-- e)
merge :: (Ord a) => ([a],[a]) -> [a]
merge ([],right) = right
merge (left,[]) = left
merge (x:xs,y:ys) =
	if(x<y)
	then x:merge(xs,y:ys)
	else y:merge(x:xs,ys)


-- f)
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort( fst (split xs) ),  mergeSort( snd(split xs)))


