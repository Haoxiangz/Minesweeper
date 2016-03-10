module AISolver ( getCoastalPathes ) where

import Util

import Data.Set             ( Set )
import Data.Set as Set      ( foldr, null, fromList, notMember, union, intersection, size
                            , toAscList, member )

import Data.List as List    ( filter, foldr )

import Data.Sequence        ( Seq )
import Data.Sequence as Seq ( empty, filter, (<|), update, index, findIndexL, drop, take
                            , (><), mapWithIndex )

isNeighbour :: Point -> Point -> Bool
isNeighbour (r1, c1) (r2, c2) =
    ((r1 /= r2) || (c1 /= c2)) && abs (r1 - r2) <= 1 && abs (c1 - c2) <= 1

-- | Find all unrevealed neighbours of an opened Point as a Set for
-- all open Points, and put all the sets in a Sequence.
classifyNeighboursByOpens :: Int -> Int -> Set Point -> Seq (Set Point)
classifyNeighboursByOpens w h opens =
    Seq.filter (not . Set.null) $
        Set.foldr (\p acc -> unrevealNeighboursOf p <| acc) Seq.empty opens
            where gridPs = fromList $ gridPoints w h
                  unrevealNeighboursOf p = fromList $
                      List.filter (\a -> a `notMember` opens && a `member` gridPs) (neighboursOf p)

-- | Make continuous Points in a group, and return all these groups in a Sequence.
groupContinuousPs :: Seq (Set Point) -> Int -> Seq (Set Point)
groupContinuousPs seq location | location >= length seq - 1      = seq
                               | Just n <- findGroupNeighbour ed =
                                     groupContinuousPs (Seq.take location seq ><
                                                            Seq.update n (st `union` (ed `index` n)) ed) location
                               | otherwise                       = groupContinuousPs seq (location + 1)
    where st                   = seq `Seq.index` location
          ed                   = Seq.drop (location + 1) seq
          isContinuous sp1 sp2 = size (sp1 `intersection` sp2) > 0
          findGroupNeighbour   = findIndexL (isContinuous st)

seqSetToSeqList :: Seq (Set Point) -> Seq [Point]
seqSetToSeqList = Seq.mapWithIndex (\_ sp -> toAscList sp)

-- | Get continuous Points in a group, and return all these groups in a list.
getCoastalPathes :: Int -> Int -> Set Point -> Seq [Point]
getCoastalPathes w h opens = seqSetToSeqList $
    groupContinuousPs (classifyNeighboursByOpens w h opens) 0

-- |
backTracking :: [Point] -> Set Point -> Point
backTracking path opens = List.foldr