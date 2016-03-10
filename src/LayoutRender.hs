module LayoutRender ( drawPlay, drawOver ) where

import Util

import Text.Tabular
import Text.Tabular.AsciiArt ( render )

import Data.List.Split       ( chunksOf )

import Data.Set              ( Set )
import Data.Set as Set       ( member )

-- | Put constructor Header on each element in sourceList.
rangeHeader :: Int -> [String] -> [Header String]
rangeHeader len sourceList = take len $ map Header sourceList

-- | Draw the game's layout according to the convert function and
-- the array of number of neighbour mines of each Point.
draw :: (Point -> String) -> [[Int]] -> IO ()
draw convert nums = putStr $ render id id id gridLayout
    where (w, h)    = dimension nums

          grid :: [[Point]]
          grid = chunksOf w $ gridPoints w h

          gridLayout :: Table String String String
          gridLayout = Table
              (Group SingleLine
                  [ Group SingleLine $ rangeHeader h [[c] | c <- rows] ])
              (Group SingleLine
                  [ Group SingleLine $ rangeHeader w [show n | n <- [0..]] ])
              (map (map convert) grid)

-- | Draw the game's layout according to the open Points and
-- the array of number of neighbour mines of each Point.
drawPlay opens nums = draw convert nums
    where -- Convert a Point position to its representation,
          -- either black block or number of neighbour mines.
          convert :: Point -> String
          convert p@(r, c) | p `member` opens = show $ nums !! r !! c
                           | otherwise        = ['\x2588']

-- | Draw the game over layout.
drawOver :: Set Point -> [[Int]] -> IO ()
drawOver minePs nums = draw convert nums
        where -- Convert a Point position to its representation in
              -- String, either black block or number of neighbour mines.
              convert :: Point -> String
              convert p@(r, c) | p `member` minePs = "*"
                               | otherwise         = show $ nums !! r !! c
