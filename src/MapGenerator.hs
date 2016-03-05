module MapGenerator
( minesMap
, rands
, randomGrid
) where

import Data.Set (Set)
import Data.Set as Set (size, insert, empty, member)

import Data.List.Split (chunksOf)

import System.Random (randomRs, mkStdGen, split)

import System.IO.Unsafe ( unsafePerformIO )
import System.Time ( getClockTime , ClockTime(..) )

type Point = (Int, Int)


-- | Generate the location of mines.
minesMap :: Int -> Int -> Int -> Int -> [[Bool]]
minesMap seed w h num = if num > maxMines
                                then error ("Number of mines should less then " ++ show maxMines)
                                else convert points
    where
        maxMines = w * h `quot` 2
        points = collect (Set.empty) (rands seed w h)

        -- | Collect num non-repetitive Points.
        collect :: Set Point -> [Point] -> Set Point
        collect ps (h:t)
            | Set.size ps >= num = ps
            | otherwise            = if h `member` ps
                                        then collect ps t
                                        else collect (insert h ps) t

        -- | This function converts the location of mine is
        -- based on the assumption of Point's index as follows, where w=3 and h=4:
        -- 00 01 02
        -- 10 11 12
        -- 20 21 22
        -- 30 31 32
        convert :: Set Point -> [[Bool]]
        convert ps = chunksOf w $ map (`member` ps) [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]

rands seed w h =
    let (gw, gh) = split (mkStdGen seed)
        xs       = randomRs (0, w - 1) gw
        ys       = randomRs (0, h - 1) gh
    in zip xs ys


sessionSeed :: Int
sessionSeed = fromIntegral (case (unsafePerformIO getClockTime) of (TOD s m) -> s)

randomGrid :: Int -> Int -> Int -> [[Bool]]
randomGrid = minesMap sessionSeed