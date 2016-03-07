module MapGenerator
( minesMap
) where

import Grid

import Data.Set (Set)
import Data.Set as Set (size, insert, empty, member)

import Data.List.Split (chunksOf)

import System.Random (randomRs, mkStdGen, split)

-- | Generate the mine map with 1 has mine in that Point while 0 doesn't.
minesMap :: Int -> Int -> Int -> Int -> [[Int]]
minesMap seed w h num = if num > maxNum
                                then error $ "Number of mines should less then " ++ show maxNum
                                else if w > length rows
                                    then error $ "Number of columns should less then " ++ (show $ length rows)
                                    else convert points
    where
        maxNum = w * h `quot` 2

        points :: Set Point
        points = collect (Set.empty) (rands seed w h)

        -- | Collect num non-repetitive Points.
        collect :: Set Point -> [Point] -> Set Point
        collect ps (h:t)
            | Set.size ps >= num = ps
            | otherwise            = if h `member` ps
                                        then collect ps t
                                        else collect (insert h ps) t

        -- | This function converts the Point of mine based on the assumption
        -- of Point's index as follows, where w=3 and h=4:
        -- (0, 0) (0, 1) (0, 2)
        -- (1, 0) (1, 1) (1, 2)
        -- (2, 0) (2, 1) (2, 2)
        -- (3, 0) (3, 1) (3, 2)
        convert :: Set Point -> [[Int]]
        convert ps = chunksOf w $
            map (\p -> fromEnum $ p `member` ps) (gridPoints w h)

-- | Produce an infinite list of random Points
rands :: Int -> Int -> Int -> [Point]
rands seed w h =
    let (gw, gh) = split (mkStdGen seed)
        xs       = randomRs (0, w - 1) gw
        ys       = randomRs (0, h - 1) gh
    in zip xs ys