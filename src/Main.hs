module Main (main) where

import Data.List (elemIndex)
import Data.List.Split (splitOneOf, chunksOf)
import Data.Char (toUpper)

import Data.Set (Set)
import Data.Set as Set (member, notMember, size, insert, empty)

import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)
import System.Time (getClockTime , ClockTime(..))

import Grid
import MapGenerator (minePoints)
import LayoutRender (draw)

-- | Calculate the number of surrounding mines for each point.
neighbourMines :: Set Point -> Int -> Int -> [[Int]]
neighbourMines ps w h = chunksOf w $ map neighbourMinesOf (gridPoints w h)
    where neighbourMinesOf :: Point -> Int
          neighbourMinesOf s = foldr (\p acc -> acc + (fromEnum $ member p ps)) 0 (neighboursOf s)


neighboursOf :: Point -> [Point]
neighboursOf (x, y) = [(x, y - 1), (x, y + 1),
                       (x + 1, y), (x + 1, y + 1), (x + 1, y - 1),
                       (x - 1, y), (x - 1, y + 1), (x - 1, y - 1)]

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
        w      = read width  :: Int
        h      = read height :: Int
        mps    = minePoints sessionSeed w h (read num :: Int)

        counts :: [[Int]]
        counts = neighbourMines mps w h

        play :: Set Point -> IO ()
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
                    if coordinate `member` mps
                        then do
                            putStrLn "Game OVER!\n"
                        else do
                            let newOpens = uncover coordinate opens mps counts
                            if Set.size newOpens == w * h - (Set.size mps)
                                then putStrLn "Congratulations!\n"
                                else play newOpens
    play Set.empty

-- | Handle uncover event, recursively uncover neighbour Points if necessary.
uncover :: Point -> Set Point -> Set Point -> [[Int]] -> Set Point
uncover n@(x, y) opens minePoints counts
    | counts !! x !! y /= 0 = Set.insert n opens
    | otherwise             = let newOpens = Set.insert n opens
                              in foldr (\p acc -> uncover p acc minePoints counts) newOpens
                                    (safeUnopenedNeighbours n newOpens minePoints)
        where
            safeUnopenedNeighbours :: Point -> Set Point -> Set Point -> [Point]
            safeUnopenedNeighbours p opens minePoints = [(x, y) | (x, y) <- neighboursOf p,
                                                                              x >= 0 &&
                                                                              x < (length counts) &&
                                                                              y >= 0 &&
                                                                              y < (length $ head counts) &&
                                                                              (x, y) `notMember` minePoints &&
                                                                              (x, y) `notMember` opens]