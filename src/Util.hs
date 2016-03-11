module Util where

import Data.Set        ( Set )
import Data.Set as Set ( fromList, member, filter )

type Point = (Int, Int)

-- | Generate a list of Point (r, c) according to the width and height
-- of the layout in the game, with r is the index of row
-- and c is the index of column.
gridPoints :: Int -> Int -> [Point]
gridPoints w h = [(r, c) | r <- [0 .. h - 1], c <- [0 .. w - 1]]

-- | Get tuple (width, height) from a two dimension list.
dimension :: [[Int]] -> (Int, Int)
dimension a = (length $ head a, length a)

rows = ['A'..'Z']

-- | Get neighbours of a Point in a Set.
neighboursOf :: Int -> Int -> Point -> Set Point
neighboursOf w h (r, c) = Set.filter (`member` gridPs) possibleNeighbours
    where gridPs             = fromList $ gridPoints w h
          possibleNeighbours = fromList [(r, c - 1), (r, c + 1),
                                         (r + 1, c), (r + 1, c + 1), (r + 1, c - 1),
                                         (r - 1, c), (r - 1, c + 1), (r - 1, c - 1)]

numAtPoint nums (r, c) = nums !! r !! c