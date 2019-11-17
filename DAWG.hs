{-# LANGUAGE ForeignFunctionInterface #-}

module DAWG where

import Debug.Trace
import Foreign.C.Types

type Seed = Int
type Coordinate = [Int]


data Tile =
      Empty
    | Wall
    deriving (Eq, Show)

data Primitive =
      Sphere Coordinate Int
    | Box Coordinate Coordinate
    deriving (Eq, Show)

at :: Coordinate -> Int -> Int
coordinates `at` index
    | index >= length coordinates = 0
    | otherwise = coordinates !! index

zipCoords :: (Int -> Int -> a) -> Coordinate -> Coordinate -> [a]
zipCoords f [] [] = []
zipCoords f (p1:coords1) [] = (f p1 0) : (zipCoords f coords1 [])
zipCoords f [] (p2:coords2) = (f 0 p2) : (zipCoords f [] coords2)
zipCoords f (p1:coords1) (p2:coords2) = (f p1 p2) : (zipCoords f coords1 coords2)

world :: Int -> Coordinate -> Tile
world seed point = tile
    where
        primitives = [
              Box [-2, -3, 0] [2, 4, 0]
            , Box [-2, 0, -2] [5, 0, 2]
            , Sphere [5, 0, 0] 2]

        inPrimitive = foldl (||) False (map (isIn point) primitives)
        tile = if inPrimitive then Wall else Empty

isIn :: Coordinate -> Primitive -> Bool
point `isIn` (Sphere position radius) = distSquared < (radius ^ 2)
    where
        added = zipCoords (\c1 c2 -> (c1 + c2) ^ 2) point position
        distSquared = sum added
point `isIn` (Box p1 p2) = foldl (&&) True rangeResults
    where
        paddedPoint1 = p1 ++ (zeroes (length point - length p1))
        paddedPoint2 = p2 ++ (zeroes (length point - length p2))
        partial = zipCoords inRange paddedPoint1 paddedPoint2
        rangeResults = zipWith (\f p -> f p) partial point

zeroes n = take n $ [0, 0..]

inRange :: Int -> Int -> Int -> Bool
inRange a b c = (c >= l) && (c <= h)
    where
        h = if (a > b) then a else b
        l = if (a <= b) then a else b

query2d n = [([x, y], world 0 [x, y]) | x <- [-n..n], y <- [-n..n]]
query3d n = [([x, y, z], world 0 [x, y, z]) | x <- [-n..n], y <- [-n..n], z <- [-n..n]]

printWorld :: [(Coordinate, Tile)] -> String
printWorld [] = ""
printWorld ((coords, tile):xs) = element ++ printWorld xs
    where
        element = "[" ++ show coords ++ ", " ++ show tile ++ "],"

queryPoint3d :: Int -> Int -> Int -> Int
queryPoint3d x y z = case world 0 [x, y, z] of
    Empty -> 0
    Wall -> 1

query_dawg :: CInt -> CInt -> CInt -> CInt
query_dawg x y z = fromIntegral $ queryPoint3d (fromIntegral x) (fromIntegral y) (fromIntegral z)

foreign export ccall query_dawg :: CInt -> CInt -> CInt -> CInt

main = do
    print $ printWorld $ query3d 40
