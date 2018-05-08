-- head :: [a] -> a
-- tail :: [a] -> a
-- fst :: (a, b) -> a
-- shift :: ((a,b), c) -> (a, (b, c))

concatena :: [[a]] -> [a]
concatena [] = []
concatena (x:xs) = (++) x $ concatena xs

inverte :: [a] -> [a]
inverte [] = []
inverte (x:xs) = (inverte xs) ++ [x]

ultimo :: [a] -> a
ultimo [x] = x
ultimo (x:xs) = ultimo xs

inicio :: [a] -> [a]
inicio [x] = []
inicio (x:xs) = x:(inicio xs)

pega :: (Num n, Eq n) => n -> [a] -> [a]
pega 0 l = []
pega n (x:xs) = x:(pega (n-1) xs)

tira :: (Num n, Eq n) => n -> [a] -> [a]
tira 0 l = l
tira n (x:xs) = tira (n-1) xs
