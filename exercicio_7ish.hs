module Main where

myFold :: (b -> a -> b) -> b -> [a] -> b
myFold f b []     = b
myFold f b [x]    = f b x
myFold f b (x:xs) = myFold f (f b x) xs

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = (:) (f x) $ map f xs

lengthWithFold :: [a] -> Int
lengthWithFold l = myFold (\x y -> 1 + x) 0 l

lengthWithMap :: [a] -> Int
lengthWithMap l = sum $ map f l
  where
    f = (\_ -> 1)

filterString :: (Char -> Bool) -> [Char] -> [Char]
filterString _  [] = []
filterString f  (x:xs) = if f x then filterString f xs else x : (filterString f xs)

squareSum :: [Int] -> Int
squareSum = myFold (\x y -> x + (y*y)) 0
