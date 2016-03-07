module Grid where

type Point = (Int, Int)

gridPoints :: Int -> Int -> [Point]
gridPoints w h = [(x, y) | x <- [0 .. h - 1], y <- [0 .. w - 1]]

rows = ['A'..'Z']