module Main where

import Ai

import Game
import System.Random (StdGen, getStdGen)

gameLoop :: StdGen -> Int -> Maybe TurnAction -> Game -> IO ()
gameLoop _ n Nothing _ =
    putStrLn ("Game took " ++ show n ++ " rounds.")
gameLoop gen n (Just action) g = do
    putStrLn (show (getLegalMoves g) ++ " >> " ++ show action)

    let updG = playTurn g action
    print updG

    let (mm, updGen) = randomAi gen updG
    gameLoop updGen (n + 1) mm updG

playGame :: IO ()
playGame = do
    let g = newGame
    print g
    gen <- getStdGen
    let (mm, updGen) = randomAi gen g

    gameLoop updGen 0 mm g

main :: IO ()
main = do
    playGame
