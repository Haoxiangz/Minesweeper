module MapGenerator (minePoints) where

import Grid

import Data.Set (Set)
import Data.Set as Set (size, insert, empty, member)

import System.Random (randomRs, mkStdGen, split)

-- | Generate all the mine points.
minePoints :: Int -> Int -> Int -> Int -> Set Point
minePoints seed w h num = if num > maxNum
                                then error $ "Number of mines should less then " ++ show maxNum
                                else if w > length rows
                                    then error $ "Number of columns should less then " ++ (show $ length rows)
                                    else collect (Set.empty) (rands seed w h)
    where
        maxNum = w * h `quot` 2

        -- | Collect num non-repetitive Points.
        collect :: Set Point -> [Point] -> Set Point
        collect ps (h:t)
            | Set.size ps >= num = ps
            | otherwise            = if h `member` ps
                                        then collect ps t
                                        else collect (insert h ps) t

-- | Produce an infinite list of random Points
rands :: Int -> Int -> Int -> [Point]
rands seed w h =
    let (gw, gh) = split (mkStdGen seed)
        xs       = randomRs (0, w - 1) gw
        ys       = randomRs (0, h - 1) gh
    in zip xs ys