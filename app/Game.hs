module Game (Player (..), newGame, Game (..), TurnAction (..), playTurn) where

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
    Game newPlayer newPlayer Plr1sTurn

data GameState = Plr1sTurn | Plr2sTurn | Plr1Won | Plr2Won | Draw deriving (Eq)

data Game = Game {player1 :: Player, player2 :: Player, gameState :: GameState}

{- |
Example of how it looks:
------------------
| -> Plr1: (1,1) |
|    Plr2: (1,1) |
------------------
-}
instance Show Game where
    show Game{player1 = plr1, player2 = plr2, gameState = state} =
        let line = "------------------"
         in concat
                [ line
                , "\n| "
                , if state == Plr1sTurn then "-> " else "   "
                , "Plr1: "
                , show plr1
                , " |\n| "
                , if state == Plr2sTurn then "-> " else "   "
                , "Plr2: "
                , show plr2
                , " |\n"
                , case state of
                    Draw ->
                        "|  It's a Draw!  |\n"
                    Plr1Won ->
                        "|   Plr 1 won!   |\n"
                    Plr2Won ->
                        "|   Plr 2 won!   |\n"
                    _ -> ""
                , line
                ]

checkVictoryOr :: GameState -> Game -> GameState
checkVictoryOr orState Game{player1 = plr1, player2 = plr2}
    | a plr1 + b plr1 == 0 = Plr2Won
    | a plr2 + b plr2 == 0 = Plr1Won
    | otherwise = orState -- TODO: check for Draw!

data TurnAction
    = AToA
    | AToB
    | BToA
    | BToB
    | Split

{- | find out whether the action is legal
for example if going from `a to a` then check whether both players have a hands
-}
isLegal :: TurnAction -> Game -> Bool
isLegal AToA Game{player1 = plr1, player2 = plr2} =
    not (a plr1 == 0 || a plr2 == 0)
isLegal BToB Game{player1 = plr1, player2 = plr2} =
    not (b plr1 == 0 || b plr2 == 0)
isLegal AToB Game{player1 = plr1, player2 = plr2, gameState = Plr1sTurn} =
    not (a plr1 == 0 || b plr2 == 0)
isLegal AToB Game{player1 = plr1, player2 = plr2, gameState = Plr2sTurn} =
    not (b plr1 == 0 || a plr2 == 0)
isLegal BToA Game{player1 = plr1, player2 = plr2, gameState = Plr1sTurn} =
    not (b plr1 == 0 || a plr2 == 0)
isLegal BToA Game{player1 = plr1, player2 = plr2, gameState = Plr2sTurn} =
    not (a plr1 == 0 || b plr2 == 0)
isLegal Split Game{player1 = plr1, gameState = Plr1sTurn} =
    (a1 == 0 || b1 == 0) && even (a1 + b1)
  where
    a1 = a plr1
    b1 = b plr1
isLegal Split Game{player2 = plr2, gameState = Plr2sTurn} =
    (a2 == 0 || b2 == 0) && even (a2 + b2)
  where
    a2 = a plr2
    b2 = b plr2
isLegal _ _ =
    False

-- TODO: make readable with case?

-- | assumes action is legal
makeMove :: TurnAction -> Game -> Game
makeMove AToA g@Game{player1 = plr1, player2 = plr2, gameState = Plr1sTurn} =
    g{player2 = addToHandA plr2 (a plr1)}
makeMove AToB g@Game{player1 = plr1, player2 = plr2, gameState = Plr1sTurn} =
    g{player2 = addToHandB plr2 (a plr1)}
makeMove BToA g@Game{player1 = plr1, player2 = plr2, gameState = Plr1sTurn} =
    g{player2 = addToHandA plr2 (b plr1)}
makeMove BToB g@Game{player1 = plr1, player2 = plr2, gameState = Plr1sTurn} =
    g{player2 = addToHandB plr2 (b plr1)}
makeMove Split g@Game{player1 = plr1, gameState = Plr1sTurn} =
    let newHand = (a plr1 + b plr1) `div` 2
     in g{player1 = Player{a = newHand, b = newHand}}
makeMove AToA g@Game{player1 = plr1, player2 = plr2, gameState = Plr2sTurn} =
    g{player1 = addToHandA plr1 (a plr2)}
makeMove AToB g@Game{player1 = plr1, player2 = plr2, gameState = Plr2sTurn} =
    g{player1 = addToHandB plr1 (a plr2)}
makeMove BToA g@Game{player1 = plr1, player2 = plr2, gameState = Plr2sTurn} =
    g{player1 = addToHandA plr1 (b plr2)}
makeMove BToB g@Game{player1 = plr1, player2 = plr2, gameState = Plr2sTurn} =
    g{player1 = addToHandB plr1 (b plr2)}
makeMove Split g@Game{player2 = plr2, gameState = Plr2sTurn} =
    let newHand = (a plr2 + b plr2) `div` 2
     in g{player2 = Player{a = newHand, b = newHand}}
makeMove _ g =
    g

playTurn :: TurnAction -> Game -> Game
playTurn action g@Game{gameState = state} =
    case state of
        Plr1sTurn -> doTheTurn Plr2sTurn
        Plr2sTurn -> doTheTurn Plr1sTurn
        _ ->
            g
  where
    doTheTurn otherPlr =
        if isLegal action g
            then
                let movedG = makeMove action g
                 in movedG{gameState = checkVictoryOr otherPlr movedG}
            else g
