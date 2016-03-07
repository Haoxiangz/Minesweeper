{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.List ((\\), elemIndex)
import Data.List.Split (splitOneOf)
import Data.Char (toUpper)

import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)
import System.Time (getClockTime , ClockTime(..))

import Grid
import MapGenerator (minesMap)
import LayoutRender (draw)

type Count  = [[Int]]

class AddThree a where
  add3 :: a -> a -> a -> a
  zero :: a
  addOffset :: [a] -> [a]
  addOffset = zipOffset3 add3 zero
  
instance AddThree Int where
  add3 n m p = n+m+p
  zero       = 0

instance AddThree [Int] where
  add3 = zipWith3 add3
  zero = repeat zero

-- Combine elementwise (i.e. zipWith3) the three lists:
--
--     z,a0,a1,a2,...
--    a0,a1,a2,...,an
--      a1,a2,...,an,z
--
-- using the ternary function f
-- Example: f is addition of three numbers, z is zero.
zipOffset3 :: (a -> a -> a -> a) -> a -> [a] -> [a]
zipOffset3 f z xs = zipWith3 f (z:xs) xs (tail xs ++ [z])

-- | Calculate the number of surrounding mines for each point.
surroundingMines :: [[Int]] -> [[Int]]
surroundingMines = addOffset . map addOffset

-- | Generate a random seed based on time
sessionSeed :: Int
sessionSeed = fromIntegral (case (unsafePerformIO getClockTime) of (TOD s m) -> s)

-- | Validate the input string, and convert it to Point.
--
-- A valid input should be " *row *[,.;] *col *", in which
-- row is a letter which order is less then (h - 1), and
-- col is a number which is less then (w - 1).
validateInput :: String -> Int -> Int -> Maybe Point
validateInput input w h =
    let arr = splitOneOf ",.;" $ filter (/= ' ') input
    in
        if length arr /= 2
            then Nothing
            else
                let letter = arr !! 0
                in
                    if length letter /= 1
                        then Nothing
                        else
                            let [l]    = letter
                                maybeIndex = (toUpper l) `elemIndex` rows
                            in
                                if maybeIndex == Nothing
                                    then Nothing
                                    else
                                        let (Just row) = maybeIndex
                                        in
                                            if row  > h - 1
                                                then Nothing
                                                else
                                                    let numStr = arr !! 1
                                                        col = read numStr :: Int
                                                    in
                                                        if col > w - 1
                                                            then Nothing
                                                            else Just (row, col)


main :: IO ()
main = do
    [width, height, num] <- getArgs
    let
        w      = read width :: Int
        h      = read height :: Int
        mmap   = minesMap sessionSeed w h (read num :: Int)

        counts :: [[Int]]
        counts = surroundingMines mmap

        opens :: [[Bool]]
        opens  = map (map (const False)) mmap

        play :: [[Bool]] -> IO ()
        play opens = do
            draw opens counts
            putStr "Input next uncover coordinate as \"row, column\": "
            input <- getLine
            let maybeCoordinate = validateInput input w h
            if maybeCoordinate == Nothing
                then do
                    putStrLn "Invalid coordiate, please input again.\n"
                    play opens
                else do
                    putStr "\n"
                    let (Just coordinate) = maybeCoordinate
                    if mmap !! (fst coordinate) !! (snd coordinate) == 1
                        then do
                            putStrLn "Game OVER!\n"
                            return ()
                        else
                            play (uncoverNbhrs counts [coordinate] coordinate opens)
    play opens





-- Transitively uncover all the neighbours of all the points in a list.
-- Repeatedly applies uncoverNbhrs
uncoverNbhrsList :: [[Int]] -> [Point] -> [Point] -> 
                    [[Bool]] -> [[Bool]]
uncoverNbhrsList count avoid
  = foldr (.) id . map (uncoverNbhrs count avoid)

-- Transitively uncover all the neighbours of a point.
-- First uncover the immediate neighbours, then call recursively on
-- all the neighbours with zero adjacency count.
uncoverNbhrs :: [[Int]] -> [Point] -> Point -> 
                [[Bool]] -> [[Bool]]
uncoverNbhrs count avoid (p,q)
  = uncoverNbhrsList count (avoid++nbhrs count (p,q)) 
                           (nullNbhrs count (p,q) \\ avoid) 
    .
    ( foldr (.) id $ 
      map ((flip.uncurry) updateArray True) (nbhrs count (p,q)) )

-- Update an array to have value x at position (n,m)
updateArray :: Int -> Int -> a -> [[a]] -> [[a]]
updateArray n m x xss = update n (update m (const x)) xss

-- Update list xs at index n to have value f (xs!!n)
-- Handles out of range indices
update :: Int -> (a -> a) -> [a] -> [a]
update n f xs = front ++ rear
        where
        (front,rest) = splitAt n xs
        rear = case rest of
            []    -> []
            (h:t)    -> f h:t


-- What are the neighbours of a point?
nbhrs :: [[Int]] -> Point -> [Point]
nbhrs count (p,q)
  = filter inGrid [ (p-1,q-1), (p-1,q), (p-1,q+1),
                    (p,q-1),   (p,q),   (p,q+1),
            (p+1,q-1), (p+1,q), (p+1,q+1) ]
    where
    inGrid (s,t) = 0<=s && s <= rows &&
                   0<=t && t <= cols
    rows = length count - 1
    cols = length (head count) -1


-- What are the null nbhrs?
nullNbhrs :: [[Int]] -> Point -> [Point]
nullNbhrs count (p,q)
  = filter zeroVal (nbhrs count (p,q))
    where
    zeroVal (s,t) = count!!s!!t==0