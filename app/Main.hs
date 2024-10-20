module Main where

import Game

gameLoop :: Int -> Game -> IO ()
gameLoop 0 g =
    print g
gameLoop n g = do
    print g
    gameLoop (n - 1) (aToA g)

main :: IO ()
main = do
    -- let a = newPlayer
    -- print a
    -- gameLoop 7 newGame

    let g = newGame
    print g
    let g2 = aToA g
    print g2
    let g3 = aToB g2
    print g3
    let g4 = bToA g3
    print g4
    let g5 = bToB g4
    print g5
