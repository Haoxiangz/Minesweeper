module LayoutRender (draw) where

import Grid

import Text.Tabular
import Text.Tabular.AsciiArt (render)

import Data.List.Split (chunksOf)

-- | Put constructor Header on each element in sourceList.
rangeHeader :: Int -> [String] -> [Header String]
rangeHeader len sourceList = take len $ map (\s -> Header s) sourceList

-- | Draw the game's layout according to the open points and
-- the number of surrounding mines for each point.
draw :: [[Bool]] -> [[Int]] -> IO ()
draw opens nums = putStr $ render id id id $ gridLayout
    where
        w    = length $ head opens
        h    = length opens

        grid :: [[Point]]
        grid = chunksOf w $ gridPoints w h

        -- | Convert a Point position to its representation,
        -- either black block or number of mines, according to
        -- the open status array (opens) and the number of
        -- mines array (nums).
        convert :: Point -> String
        convert (x, y) | opens !! x !! y = show $ nums !! x !! y
                       | otherwise       = ['\x2588']

        gridLayout :: Table String String String
        gridLayout = Table
            (Group SingleLine
                [ Group SingleLine $ rangeHeader h [[c] | c <- rows] ])
            (Group SingleLine
                [ Group SingleLine $ rangeHeader w [show n | n <- [0..]] ])
            (map (map convert) grid)