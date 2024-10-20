module Game (newGame, Game (..), aToA, aToB, bToA, bToB) where

newPlayer :: Player
newPlayer =
    Player 1 1

data Player = Player {a :: Int, b :: Int}

instance Show Player where
    show Player{a = _a, b = _b} =
        "(" ++ show _a ++ "," ++ show _b ++ ")"

addToHandA :: Player -> Int -> Player
addToHandA Player{a = a_, b = b_} n =
    Player{a = (a_ + n) `mod` 5, b = b_}

addToHandB :: Player -> Int -> Player
addToHandB Player{a = a_, b = b_} n =
    Player{a = a_, b = (b_ + n) `mod` 5}

newGame :: Game
newGame =
    Game newPlayer newPlayer True

data Game = Game {player1 :: Player, player2 :: Player, player1Turn :: Bool}

{- |
Example of how it looks:
------------------
| -> Plr1: (1,1) |
|    Plr2: (1,1) |
------------------
-}
instance Show Game where
    show Game{player1 = plr1, player2 = plr2, player1Turn = isPlr1} =
        let line = "------------------"
         in concat
                [ line
                , "\n| "
                , if isPlr1 then "-> " else "   "
                , "Plr1: "
                , show plr1
                , " |\n| "
                , if not isPlr1 then "-> " else "   "
                , "Plr2: "
                , show plr2
                , " |\n"
                , line
                ]

aToA :: Game -> Game
aToA g@Game{player1 = plr1, player2 = plr2, player1Turn = True} =
    g{player2 = addToHandA plr2 (a plr1), player1Turn = False}
aToA g@Game{player1 = plr1, player2 = plr2, player1Turn = False} =
    g{player1 = addToHandA plr1 (a plr2), player1Turn = True}

aToB :: Game -> Game
aToB g@Game{player1 = plr1, player2 = plr2, player1Turn = True} =
    g{player2 = addToHandB plr2 (a plr1), player1Turn = False}
aToB g@Game{player1 = plr1, player2 = plr2, player1Turn = False} =
    g{player1 = addToHandB plr1 (a plr2), player1Turn = True}

bToA :: Game -> Game
bToA g@Game{player1 = plr1, player2 = plr2, player1Turn = True} =
    g{player2 = addToHandA plr2 (b plr1), player1Turn = False}
bToA g@Game{player1 = plr1, player2 = plr2, player1Turn = False} =
    g{player1 = addToHandA plr1 (b plr2), player1Turn = True}

bToB :: Game -> Game
bToB g@Game{player1 = plr1, player2 = plr2, player1Turn = True} =
    g{player2 = addToHandB plr2 (b plr1), player1Turn = False}
bToB g@Game{player1 = plr1, player2 = plr2, player1Turn = False} =
    g{player1 = addToHandB plr1 (b plr2), player1Turn = True}
