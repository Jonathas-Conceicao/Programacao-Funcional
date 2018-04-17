iSort :: (Ord a) => [a] -> [a]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

ins :: (Ord a) => a -> [a] -> [a]
ins a [] = [a]
ins a (x:xs)
  | a < x     = (a:x:xs)
  | otherwise = (x:(ins a xs))

qSort :: (Ord a) => [a] -> [a]
qSort [] = []
qSort (x:xs) = qSort ([a | a <- xs, a < x]) ++ [x] ++ qSort ([a | a <- xs, a >= x])

qSort2 :: (Ord a) => [a] -> [a]
qSort2 [] = []
qSort2 (x:xs) = qSort ([a | a <- xs, a > x]) ++ [x] ++ qSort ([a | a <- xs, a < x])

minEMax :: (Ord a) => [a] -> (a, a)
minEMax l = (a,b)
  where
    a = foldr1 min l
    b = foldr1 max l

