module Main ( main ) where

import Data.List          ( elem, elemIndex )
import Data.List.Split    ( splitOneOf, chunksOf )
import Data.Char          ( toUpper )

import Data.Set           ( Set )
import Data.Set as Set    ( member, notMember, size, insert, empty )

import Data.Maybe         ( isNothing )

import System.Environment ( getArgs )

import Grid
import MapGenerator       ( minePoints )
import LayoutRender       ( draw )

-- | Calculate the number of surrounding mines for each point.
neighbourMines :: Set Point -> Int -> Int -> [[Int]]
neighbourMines minePs w h = chunksOf w $ map neighbourMinesOf (gridPoints w h)
    where neighbourMinesOf :: Point -> Int
          neighbourMinesOf s = foldr (\p acc -> acc + fromEnum (member p minePs)) 0 (neighboursOf s)


neighboursOf :: Point -> [Point]
neighboursOf (x, y) = [(x, y - 1), (x, y + 1),
                       (x + 1, y), (x + 1, y + 1), (x + 1, y - 1),
                       (x - 1, y), (x - 1, y + 1), (x - 1, y - 1)]

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
                                                    in case reads numStr :: [(Int, String)] of -- Determine if a string is int
                                                        [(col, "")] -> if col > w - 1
                                                                            then Nothing
                                                                            else Just (row, col)
                                                        _           -> Nothing


main :: IO ()
main = do
    [width, height, num] <- getArgs
    let w = read width  :: Int
        h = read height :: Int
    case minePoints w h (read num :: Int) of
        Left err  -> putStrLn err
        Right mps -> let nums :: [[Int]]
                         nums = neighbourMines mps w h

                         play :: Set Point -> IO ()
                         play opens = do
                             draw opens nums
                             putStr "Input next uncover coordinate as \"row, column\": "
                             input <- getLine
                             let maybeCoordinate = validateInput input w h
                             if isNothing maybeCoordinate
                                 then do
                                     putStrLn "Invalid coordiate, please input again.\n"
                                     play opens
                                 else do
                                     putStr "\n"
                                     let (Just coordinate) = maybeCoordinate
                                     if coordinate `member` mps
                                         then putStrLn "Game OVER!\n"
                                         else do
                                             let newOpens = uncover coordinate opens mps nums
                                             if Set.size newOpens == w * h - Set.size mps
                                                 then putStrLn "Congratulations!\n"
                                                 else play newOpens
                     in play Set.empty

-- | Handle uncover event, recursively uncover neighbour Points if necessary.
uncover :: Point -> Set Point -> Set Point -> [[Int]] -> Set Point
uncover n@(x, y) opens minePs nums
    | n `member` opens    = opens   -- Point n already opened
    | nums !! x !! y /= 0 = Set.insert n opens
    | otherwise             = let newOpens = Set.insert n opens
                              in foldr (\p acc -> uncover p acc minePs nums) newOpens
                                    (safeUnopenedNeighbours n newOpens minePs)
        where
            allPoints = gridPoints (length $ head nums) (length nums)

            safeUnopenedNeighbours :: Point -> Set Point -> Set Point -> [Point]
            safeUnopenedNeighbours p opens minePs = [(x, y) | (x, y) <- neighboursOf p,
                                                                  (x, y) `elem` allPoints &&
                                                                  (x, y) `notMember` minePs &&
                                                                  (x, y) `notMember` opens]