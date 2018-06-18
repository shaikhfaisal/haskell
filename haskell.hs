

circle_area :: (Num a, Ord a, Floating a) => a -> a
circle_area radius
        | radius <= zero = 0
        | radius > zero  = pi * radius_squared
        where radius_squared = radius^2
              zero = 0

initials :: String -> String -> String
initials (f:_) (l:_)
         = [f] ++ "." ++ [l]
