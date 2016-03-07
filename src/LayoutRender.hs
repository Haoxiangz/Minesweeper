module LayoutRender ( draw ) where

import Grid

import Text.Tabular
import Text.Tabular.AsciiArt ( render )

import Data.List.Split       ( chunksOf )

import Data.Set (Set)
import Data.Set as Set       ( member )

-- | Put constructor Header on each element in sourceList.
rangeHeader :: Int -> [String] -> [Header String]
rangeHeader len sourceList = take len $ map Header sourceList

-- | Draw the game's layout according to the open points and
-- the number of neighbour mines for each point.
draw :: Set Point -> [[Int]] -> IO ()
draw opens nums = putStr $ render id id id gridLayout
    where
        w    = length $ head nums
        h    = length nums

        grid :: [[Point]]
        grid = chunksOf w $ gridPoints w h

        -- | Convert a Point position to its representation,
        -- either black block or number of mines, according to
        -- the open status list (opens) and the number of
        -- number of surrounding mines (nums).
        convert :: Point -> String
        convert p@(x, y) | p `member` opens = show $ nums !! x !! y
                         | otherwise        = ['\x2588']

        gridLayout :: Table String String String
        gridLayout = Table
            (Group SingleLine
                [ Group SingleLine $ rangeHeader h [[c] | c <- rows] ])
            (Group SingleLine
                [ Group SingleLine $ rangeHeader w [show n | n <- [0..]] ])
            (map (map convert) grid)