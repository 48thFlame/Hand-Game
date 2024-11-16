module Ai (randomAi, allPositions, endingPositions, makeAPTree) where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Bifunctor
import Data.Function ((&))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

-- import Debug.Trace (trace, traceShow)
-- import qualified GHC.Num as Set
import Game
import System.Random (Random (randomR), StdGen)

randomElement :: StdGen -> [TurnAction] -> (TurnAction, StdGen)
randomElement gen list =
    let (index, newGen) = randomR (0, length list - 1) gen
     in (list !! index, newGen)

randomAi :: StdGen -> Game -> (Maybe TurnAction, StdGen)
randomAi gen g =
    let moves = getLegalMoves g
     in if not (null moves)
            then
                let (m, updGen) = randomElement gen moves
                 in (Just m, updGen)
            else
                (Nothing, gen)

allPositions :: Set PositionHash
allPositions =
    genAllPositions Set.empty [startHash]

genAllPositions :: Set PositionHash -> [PositionHash] -> Set PositionHash
genAllPositions seenPos [] = seenPos
genAllPositions seenPos (pos : unexplored) =
    if Set.member pos seenPos
        then
            -- do nothing, just keep the recursion going
            genAllPositions seenPos unexplored
        else
            genAllPositions
                (Set.insert pos seenPos)
                (doGameGeneration pos ++ unexplored)

doGameGeneration :: PositionHash -> [PositionHash]
doGameGeneration pos =
    let g = unHashGame pos
     in map (hashGame . playTurn g) (getLegalMoves g)

endingPositions :: Set PositionHash
endingPositions =
    Set.filter (> 29999) allPositions

-- | {position : (origins, evalScore)}
type APTree = Map PositionHash (Set PositionHash, Int)

makeAPTree :: APTree
makeAPTree =
    let emptyAPT = Map.fromSet (const (Set.empty, 0)) allPositions
     in insertAPTOrigins emptyAPT
            & insertAPTscore

{- |
These function generate an `apt`
by going threw `allPositions` and for each pos check its children
and for each child update the map with adding the origin position.
This is done with 2 folds.
-}
insertAPTOrigins :: APTree -> APTree
insertAPTOrigins apt =
    List.foldl'
        updateAPTmapOrigins
        apt
        allPositions

{- |
for each child of pos, update that location and add origin
-}
updateAPTmapOrigins :: APTree -> PositionHash -> APTree
updateAPTmapOrigins apt originPos =
    List.foldl'
        ( \newestApt posToUpdate ->
            let mapPosToUpdateItem = newestApt Map.! posToUpdate
             in Map.insert
                    posToUpdate
                    (Data.Bifunctor.first (Set.insert originPos) mapPosToUpdateItem)
                    newestApt
        )
        apt
        (doGameGeneration originPos)

insertAPTscore :: APTree -> APTree
insertAPTscore apt =
    List.foldl'
        ( \newestApt posToUpdate ->
            let mapPosToUpdateItem = newestApt Map.! posToUpdate
             in Map.insert
                    posToUpdate
                    ( Data.Bifunctor.second
                        ( const $
                            if posToUpdate > 39999
                                then
                                    (-1)
                                else -- its in the 3000's => Plr1Won
                                    1
                        )
                        mapPosToUpdateItem
                    )
                    newestApt
        )
        apt
        endingPositions

{-
If at end position - return
otherwise - check every possiblity
-}
-- minmaxAi :: Game -> Int
-- minmaxAi g@Game{gameState = state, history = hist} =
--     trace ("Current history size: " ++ show (List.length hist)) $
--         case state of
--             Draw ->
--                 0
--             Plr1Won ->
--                 1
--             Plr2Won ->
--                 (-1)
--             Plr1sTurn ->
--                 getLegalMoves g
--                     & List.map (playTurn g)
--                     & List.map (minmaxAi)
--                     & List.maximum
--             Plr2sTurn ->
--                 getLegalMoves g
--                     & List.map (playTurn g)
--                     & List.map (minmaxAi)
--                     & List.minimum