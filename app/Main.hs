module Main where

import Game

gameLoop :: Int -> Game -> IO ()
gameLoop 0 g =
    print g
gameLoop n g = do
    print g

    gameLoop (n - 1) (playTurn AToB g)

main :: IO ()
main = do
    -- let a = newPlayer
    -- print a
    gameLoop 9 newGame

-- gameLoop 2 newGame{player2 = Player{b = 0, a = 4}, player1 = Player{b = 2, a = 0}}

-- let g = newGame
-- print g
-- let g2 = playTurn AToA g
-- print g2
-- let g3 = playTurn AToB g2
-- print g3
-- let g4 = playTurn BToA g3
-- print g4
-- let g5 = playTurn BToB g4
-- print g5
