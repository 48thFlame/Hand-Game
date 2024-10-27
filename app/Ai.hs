module Ai (randomAi, randomElement) where

import Game
import System.Random

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