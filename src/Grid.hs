module Grid where

type Point = (Int, Int)

-- | Generate a list of Point (r, c) according to the width and height
-- of the layout in the game, with r is the index of row
-- and c is the index of column.
gridPoints :: Int -> Int -> [Point]
gridPoints w h = [(r, c) | r <- [0 .. h - 1], c <- [0 .. w - 1]]

rows = ['A'..'Z']