doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x <= 100
                      then doubleMe x
                      else x


sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "three!"
sayMe x = "sorry...."



myfactorial :: Int -> Int
myfactorial 0 = 1
myfactorial n = n * myfactorial(n - 1)

head' :: [a] -> a
head' []   = error "cant pop off from an empty list"
head' (x:_) = x


length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs


max' :: (Ord a) => a -> a -> a
max'  a b
          | a > b     = a
          | otherwise = b



min' :: (Ord a) => a -> a-> a
min' a b
     | a > b     = b
     | otherwise = a


chain :: (Integral a) => a -> [a]
chain 1      = [1]
chain x
    | even x  = x : chain y
    | odd x   = x : chain z
    where y = x `div` 2
          z = x * 3 + 1


numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..10]))
            where isLong xs = length xs > 15


sum' :: (Num a) => [a] -> a
sum' xs = foldl1 (\x y -> x * y ) xs

