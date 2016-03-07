module MapGenerator ( minePoints ) where

import Grid

import Data.Set         ( Set )
import Data.Set as Set  ( size, insert, empty, member )

import System.Random    ( randomRs, newStdGen, split )

import System.IO.Unsafe ( unsafePerformIO )

-- | Generate all the mine points.
minePoints :: Int -> Int -> Int -> Either String (Set Point)
minePoints w h mines | mines > maxMines = Left $ "Number of mines should less then " ++ show maxMines
                     | h > maxHeight    = Left $ "Number of rows should no large then " ++ show maxHeight
                     | otherwise        = Right $ collect Set.empty (rands w h)
    where
        maxMines = w * h `quot` 2
        maxHeight = length rows

        -- | Collect mines non-repetitive Points.
        collect :: Set Point -> [Point] -> Set Point
        collect ps (h:t)
            | Set.size ps >= mines = ps
            | otherwise            = if h `member` ps
                                        then collect ps t
                                        else collect (insert h ps) t

-- | Produce an infinite list of random Points
rands :: Int -> Int -> [Point]
rands w h =
    let (gw, gh) = split $ unsafePerformIO newStdGen
        rs       = randomRs (0, w - 1) gw
        cs       = randomRs (0, h - 1) gh
    in zip cs rs