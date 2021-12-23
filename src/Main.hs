module Main ( main ) where

import Data.List          ( elemIndex )
import Data.List.Split    ( splitOneOf, chunksOf )
import Data.Char          ( toUpper )

import Data.Set           ( Set )
import Data.Set as Set    ( member, notMember, size, insert, empty, null, foldr, filter)

import Data.Maybe         ( isNothing )
import Control.Monad      ( forM, forM_, when)

import System.Environment ( getArgs )
import System.Exit        ( exitSuccess)
import System.Random      ( randomR, mkStdGen )

import Util
import MapGenerator       ( minePoints )
import LayoutRender       ( drawPlay, drawOver )
import AISolver           ( showAllPossibleSafePoints, nextMove )

-- | Calculate the number of surrounding mines for each point.
neighbourMines :: Set Point -> Int -> Int -> [[Int]]
neighbourMines minePs w h = chunksOf w $ map neighbourMinesOf (gridPoints w h)
    where neighbourMinesOf :: Point -> Int
          neighbourMinesOf s = Set.foldr (\p acc -> acc + fromEnum (member p minePs)) 0 (neighboursOf w h s)

-- | Control the whole interactive process during game.
play :: Set Point -> Set Point -> [[Int]] -> Int -> IO ()
play opens minePs nums count = do
    drawPlay opens nums
    let (w, h) = dimension nums
    if Set.null minePs
        then do
            -- Just start at some random point
            let 
                gen = mkStdGen 42
                row = fst $ randomR (0, w-1) gen
                col = fst $ randomR (0, h-1) gen
                point   = (3,3)             -- or you could use row col for random start 
                minePs' = minePoints w h count point
                nums'   = neighbourMines minePs' w h
            print point
            update opens minePs' nums' count [point,point]
        else do
            let safe_moves = nextMove w h opens nums
            if not $ Prelude.null safe_moves
                then do 
                    putStrLn $ showAllPossibleSafePoints w h opens nums
                    update opens minePs nums count safe_moves
                else do
                    putStrLn "No more safe moves.\n"


-- | An extract function from play. This function is responsible for
-- updating the open state of Points and the game layout.
update :: Set Point -> Set Point -> [[Int]] -> Int -> [Point] -> IO ()
update opens minePs nums count points@(point:xs) =
    if point `member` minePs
        then do
            drawOver minePs nums
            putStrLn "Game OVER! You may want to try again?\n"
            exitSuccess
        else do
            let newOpens = reveal point opens minePs nums
                (w, h) = dimension nums
            if size newOpens == w * h - size minePs
                then do
                    drawPlay newOpens nums
                    putStrLn "Congratulations!\n"
                    exitSuccess
                else do
                    if Prelude.null xs
                        then do play newOpens minePs nums count
                    else do update newOpens minePs nums count xs
update opens minePs nums count points = 
    do print "Error in update: no points received"
        

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