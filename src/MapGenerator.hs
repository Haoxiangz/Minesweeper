module MapGenerator ( minePoints ) where

import Util

import Data.Set         ( Set )
import Data.Set as Set  ( size, insert, empty, member )

import System.Random    ( randomRs, newStdGen, mkStdGen, setStdGen, getStdGen, split )

import System.IO.Unsafe ( unsafePerformIO )

-- | Generate all the mine Points excluding the initial point.
minePoints :: Int -> Int -> Int -> Point -> Set Point
minePoints w h count point = collect empty (rands w h)
    where  -- Collect mines non-repetitive Points.
            collect :: Set Point -> [Point] -> Set Point
            collect ps (x:xs)
               | size ps >= min count (w * h - 1) = ps  -- the max available mine positions
                                                            -- is w * h - 1, where 1 is the
                                                            -- initial point
               | otherwise                        = if x `member` ps || x == point
                                                        then collect ps xs
                                                        else collect (insert x ps) xs

-- | Produce an infinite list of random Points
rands :: Int -> Int -> [Point]
rands w h =
    let 
        -- for benchmarking use seeded mkStdGen
        (gw, gh) = split $ unsafePerformIO $ newStdGen -- mkStdGen 10342
        rs       = randomRs (0, w - 1) gw
        cs       = randomRs (0, h - 1) gh
    in zip cs rs

