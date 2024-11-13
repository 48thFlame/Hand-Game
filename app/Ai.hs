module Ai (randomAi, allPositions) where

import Data.Set (Set)
import qualified Data.Set as Set

-- import Data.Function ((&))
-- import qualified Data.List as List
-- import Data.Map (Map)
-- import qualified Data.Map as Map
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
    genAllPositions
        (Set.insert pos seenPos)
        (doGameGeneration seenPos pos ++ unexplored)

doGameGeneration :: Set PositionHash -> PositionHash -> [PositionHash]
doGameGeneration seenSet pos =
    if Set.member pos seenSet
        then
            []
        else
            let g = unHashGame pos
             in map (hashGame . playTurn g) (getLegalMoves g)

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