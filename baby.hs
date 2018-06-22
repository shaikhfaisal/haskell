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


data Shape = Circle Float Float Float | Rectangle Float Float Float Float

surfaceArea :: Shape -> Float
surfaceArea (Circle _ _ r)          = 2 * pi * r 
surfaceArea (Rectangle x1 y1 x2 y2) = abs(x1 - x2) * abs(y1 - y2)

data Vector a = Vector a a a deriving (Show)

addVectors :: (Num a) => Vector a -> Vector a -> Vector a
addVectors (Vector x1 y1 z1) (Vector x2 y2 z2)  = Vector (x1+x2) (y1+y2) (z1+z2)

data Person = Person {
                        firstName :: String,
                        lastName :: String,
                        age :: Int
                     } deriving (Eq, Show)
