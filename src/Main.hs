module Main ( main ) where

import Data.List          ( elemIndex )
import Data.List.Split    ( splitOneOf, chunksOf )
import Data.Char          ( toUpper )

import Data.Set           ( Set )
import Data.Set as Set    ( member, notMember, size, insert, empty, null, foldr, filter)

import Data.Maybe         ( isNothing )

import System.Environment ( getArgs )

import Util
import MapGenerator       ( minePoints )
import LayoutRender       ( drawPlay, drawOver )
import AISolver           ( showAllPossibleSafePoints )

-- | Calculate the number of surrounding mines for each point.
neighbourMines :: Set Point -> Int -> Int -> [[Int]]
neighbourMines minePs w h = chunksOf w $ map neighbourMinesOf (gridPoints w h)
    where neighbourMinesOf :: Point -> Int
          neighbourMinesOf s = Set.foldr (\p acc -> acc + fromEnum (member p minePs)) 0 (neighboursOf w h s)

data InputType =
    Type Point | Help | Error deriving (Eq)

-- | Validate the input string.
--
-- A valid input should be "help", or " *row *[,.;] *col *",
-- in which row is a letter which order is less then (h - 1),
-- and col is a number which is less then (w - 1).
validateInput :: String -> Int -> Int -> InputType
validateInput input w h =
    let cleanInput = Prelude.filter (/= ' ') input
    in
        if (not $ Prelude.null cleanInput) && (toUpper $ head cleanInput) == '?'
            then Help
            else
                let arr = splitOneOf ",.;" cleanInput
                in
                    if length arr /= 2
                        then Error
                        else
                            let letter = head arr
                            in
                                if length letter /= 1
                                    then Error
                                    else
                                        let [l] = letter
                                            maybeIndex = toUpper l `elemIndex` rows
                                        in
                                            if isNothing maybeIndex
                                                then Error
                                                else
                                                    let (Just row) = maybeIndex
                                                    in
                                                        if row  > h - 1
                                                            then Error
                                                            else
                                                                let numStr = arr !! 1
                                                                in case reads numStr :: [(Int, String)] of -- Determine if a string is Int
                                                                    [(col, "")] -> if col > w - 1
                                                                                        then Error
                                                                                        else Type (row, col)
                                                                    _           -> Error

-- | Control the whole interactive process during game.
play :: Set Point -> Set Point -> [[Int]] -> Int -> IO ()
play opens minePs nums count = do
    drawPlay opens nums
    putStr "Input next point to reveal as \"row, column\": "
    input <- getLine
    let (w, h) = dimension nums
        inputType = validateInput input w h
    if inputType == Error || (inputType == Help && Set.null minePs)
        then do
            putStrLn "Invalid coordiate, please input again.\n"
            play opens minePs nums count

        -- Only able to show possible safe Points after the first reveal.
        else if inputType == Help
            then do
                putStrLn $ showAllPossibleSafePoints w h opens nums
                play opens minePs nums count
            else do
                putStrLn ""
                let Type point = inputType
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
reveal n opens minePs nums
    | n `member` opens       = opens   -- Point n already opened
    | numAtPoint nums n /= 0 = insert n opens
    | otherwise              = let newOpens = insert n opens
                               in Set.foldr (\p acc -> reveal p acc minePs nums) newOpens
                                     (safeUnopenedNeighbours n newOpens minePs)
        where
            (w, h) = dimension nums

            safeUnopenedNeighbours :: Point -> Set Point -> Set Point -> Set Point
            safeUnopenedNeighbours p opens minePs =
                Set.filter (\nb -> nb `notMember` minePs && nb `notMember` opens) (neighboursOf w h p)


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