module Main ( main ) where

import Data.List          ( elem, elemIndex )
import Data.List.Split    ( splitOneOf, chunksOf )
import Data.Char          ( toUpper )

import Data.Set           ( Set )
import Data.Set as Set    ( member, notMember, size, insert, empty, null)

import Data.Maybe         ( isNothing )

import System.Environment ( getArgs )

import Util
import MapGenerator       ( minePoints )
import LayoutRender       ( drawPlay, drawOver )
import AISolver           ( getCoastalPathes )

-- | Calculate the number of surrounding mines for each point.
neighbourMines :: Set Point -> Int -> Int -> [[Int]]
neighbourMines minePs w h = chunksOf w $ map neighbourMinesOf (gridPoints w h)
    where neighbourMinesOf :: Point -> Int
          neighbourMinesOf s = foldr (\p acc -> acc + fromEnum (member p minePs)) 0 (neighboursOf s)

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
                let letter = head arr
                in
                    if length letter /= 1
                        then Nothing
                        else
                            let [l] = letter
                                maybeIndex = toUpper l `elemIndex` rows
                            in
                                if isNothing maybeIndex
                                    then Nothing
                                    else
                                        let (Just row) = maybeIndex
                                        in
                                            if row  > h - 1
                                                then Nothing
                                                else
                                                    let numStr = arr !! 1
                                                    in case reads numStr :: [(Int, String)] of -- Determine if a string is Int
                                                        [(col, "")] -> if col > w - 1
                                                                            then Nothing
                                                                            else Just (row, col)
                                                        _           -> Nothing

-- | Control the whole interactive process during game.
play :: Set Point -> Set Point -> [[Int]] -> Int -> IO ()
play opens minePs nums count = do
    drawPlay opens nums
    putStr "Input next point to reveal as \"row, column\": "
    input <- getLine
    let (w, h) = dimension nums
        maybePoint = validateInput input w h
    if isNothing maybePoint
        then do
            putStrLn "Invalid coordiate, please input again.\n"
            play opens minePs nums count
        else do
            putStrLn ""
            let (Just point) = maybePoint
            if Set.null minePs -- Postpone the mine Points generation to the first hit
                then do
                    let minePs' = minePoints w h count point
                        nums'   = neighbourMines minePs' w h
                    update opens minePs' nums' count point
                else
                    update opens minePs nums count point

-- | An extract function from play. This function is responsible for
-- updating the open state of Points and the game layout.
update :: Set Point -> Set Point -> [[Int]] -> Int -> Point -> IO ()
update opens minePs nums count point =
    if point `member` minePs
        then do
            drawOver minePs nums
            putStrLn "Game OVER! You may want to try again?\n"
        else do
            let newOpens = reveal point opens minePs nums
                (w, h) = dimension nums
            if size newOpens == w * h - size minePs
                then do
                    drawPlay newOpens nums
                    putStrLn "Congratulations!\n"
                else play newOpens minePs nums count

-- | Handle reveal event, recursively reveal neighbour Points if necessary.
reveal :: Point -> Set Point -> Set Point -> [[Int]] -> Set Point
reveal n@(r, c) opens minePs nums
    | n `member` opens    = opens   -- Point n already opened
    | nums !! r !! c /= 0 = insert n opens
    | otherwise           = let newOpens = insert n opens
                            in foldr (\p acc -> reveal p acc minePs nums) newOpens
                                  (safeUnopenedNeighbours n newOpens minePs)
        where
            (w, h) = dimension nums
            allPoints = gridPoints w h

            safeUnopenedNeighbours :: Point -> Set Point -> Set Point -> [Point]
            safeUnopenedNeighbours p opens minePs = [(r, c) | (r, c) <- neighboursOf p,
                                                              (r, c) `elem` allPoints &&
                                                              (r, c) `notMember` minePs &&
                                                              (r, c) `notMember` opens]

main :: IO ()
main = do
    [width, height, count] <- getArgs
    let w         = read width  :: Int
        h         = read height :: Int
        c         = read count  :: Int

        maxMines  = w * h `quot` 2
        maxHeight = length rows
    if c > maxMines
        then putStrLn $ "Number of mines should less then " ++ show maxMines
        else if h > maxHeight
            then putStrLn $ "Number of rows should no greater then " ++ show maxHeight
            else play empty empty (replicate h (replicate w 0)) c