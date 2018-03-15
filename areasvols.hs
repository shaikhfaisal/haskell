module Geometry (
 areaOfCircle,
 areaOfSquare,
 areaOfRectangle,
) where


areaOfCircle ::  Float -> Float
areaOfCircle r = pi * (^2)  r

areaOfSquare :: Float -> Float
areaOfSquare length = areaOfRectangle length length

areaOfRectangle :: Float -> Float -> Float
areaOfRectangle length width = length * width


volumeOfSphere :: Float -> Float
volumeOfSphere r = 4/3 * pi * (^3) r

volumeOfCuboid :: Float -> Float -> Float -> Float
volumeOfCuboid length width breadth = length * width * breadth

volumeOfCube :: Float -> Float
volumeOfCube length = volumeOfCuboid length length length
