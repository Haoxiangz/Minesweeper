module AISolver ( showAllPossibleSafePoints ) where

import Util

import Data.Set             ( Set )
import Data.Set as Set      ( foldr, null, notMember, union, intersection, size, toAscList
                            , insert, empty, member, filter )

import Data.Sequence        ( Seq )
import Data.Sequence as Seq ( empty, filter, (<|), update, index, findIndexL, drop, take
                            , (><), mapWithIndex, foldrWithIndex, length )

-- | Find all unrevealed neighbours of an opened Point as a Set for
-- all open Points, and put all the sets in a Sequence.
classifyNeighboursByOpens :: Int -> Int -> Set Point -> Seq (Set Point)
classifyNeighboursByOpens w h opens =
    Seq.filter (not . Set.null) $
        Set.foldr (\p acc -> unrevealNeighboursOf p <| acc) Seq.empty opens
            where unrevealNeighboursOf p = Set.filter (`notMember` opens) (neighboursOf w h p)

-- | Make continuous Points in a group, and return all these groups in a Sequence.
groupContinuousPs :: Seq (Set Point) -> Int -> Seq (Set Point)
groupContinuousPs seq location | location >= Seq.length seq - 1      = seq
                               | Just n <- findGroupNeighbour ed =
                                     groupContinuousPs (Seq.take location seq ><
                                                            Seq.update n (st `union` (ed `index` n)) ed) location
                               | otherwise                       = groupContinuousPs seq (location + 1)
    where st                   = seq `Seq.index` location
          ed                   = Seq.drop (location + 1) seq
          isContinuous sp1 sp2 = Set.size (sp1 `intersection` sp2) > 0
          findGroupNeighbour   = Seq.findIndexL (isContinuous st)

seqSetToSeqList :: Seq (Set Point) -> Seq [Point]
seqSetToSeqList = Seq.mapWithIndex (\_ sp -> toAscList sp)

-- | Get continuous Points in a group, and return all these groups in a list.
getCoastalPathes :: Int -> Int -> Set Point -> Seq [Point]
getCoastalPathes w h opens = seqSetToSeqList $
    groupContinuousPs (classifyNeighboursByOpens w h opens) 0

getNeighbourOpenNumPs :: Int -> Int -> Point -> Set Point -> [[Int]] -> Set Point
getNeighbourOpenNumPs w h p opens nums =
    Set.filter isOpenNum (neighboursOf w h p)
        where isOpenNum nb = nb `member` opens && numAtPoint nums nb /= 0

-- | Try to figure out all the mine Point in the given coastal path.
backTracking :: Int -> Int -> Set Point -> [[Int]] -> [Point] -> Set Point -> Set Point
backTracking w h opens nums [] mineFlags   = mineFlags
backTracking w h opens nums (x:xs) mineFlags | verify x True  =
                                                   let flags = backTracking w h opens nums xs (x `insert` mineFlags)
                                                   in if not $ Set.null flags
                                                          then flags
                                                          else backTracking w h opens nums xs mineFlags
                                             | verify x False =
                                                   let flags = backTracking w h opens nums xs mineFlags
                                                   in if not $ Set.null flags
                                                          then flags
                                                          else backTracking w h opens nums xs (x `insert` mineFlags)
                                             | otherwise      = Set.empty
    where nbOpenNumPs p             = getNeighbourOpenNumPs w h p opens nums
          numMineFlagInNeighbours p = Set.size $ Set.filter (`member` mineFlags) (neighboursOf w h p)
          numUnknowNeighbours p     =
              Prelude.length $ Prelude.filter (`member` neighhours) xs
                  where neighhours = neighboursOf w h p

          -- Verify all the neighbour number open Point of p based on the mine assuming.
          -- If any one neighbour number open Point shows the assuming is not valid,
          -- then mine assuming is invalid.
          verify :: Point -> Bool -> Bool
          verify p@(r, c) isMine = Set.foldr (\p acc -> acc && verifyNum p isMine) True (nbOpenNumPs p)

          -- Verify if the assuming (is mine of not) is valid.
          -- p is the neighbour number open Point of the assuming Point.
          verifyNum :: Point -> Bool -> Bool
          verifyNum p isMine = numMineFlagInNeighbours p + fromEnum isMine <= numAtPoint nums p &&
                                  numAtPoint nums p <= numMineFlagInNeighbours p +
                                                       fromEnum isMine +
                                                       numUnknowNeighbours p


-- | Get possible safe Points in a coastal path.
possibleSafePoints :: Int -> Int -> Set Point -> [[Int]] -> [Point] -> [Point]
possibleSafePoints w h opens nums path = Prelude.filter (`notMember` possibleMinePoints) path
    where possibleMinePoints = backTracking w h opens nums path Set.empty

-- | For each coastal path, return possible safe Points within the path,
-- and group the results from different pathes in a list.
allPossibleSafePoints :: Int -> Int -> Set Point -> [[Int]] -> [[Point]]
allPossibleSafePoints w h opens nums = foldrWithIndex accFun [] coastalPathes
    where coastalPathes         = getCoastalPathes w h opens
          accFun index path acc = (possibleSafePoints w h opens nums path) : acc

-- | Change to all possible safe Points to printable string.
showAllPossibleSafePoints :: Int -> Int -> Set Point -> [[Int]] -> String
showAllPossibleSafePoints w h opens nums =
    "Possible safe locations: " ++ (show $ map (map pointToLoc) points) ++ "\n"
        where points            = allPossibleSafePoints w h opens nums
              pointToLoc (r, c) = (rows !! r, c)
