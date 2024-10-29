module Game (
    Player (..),
    newGame,
    Game (..),
    TurnAction (..),
    playTurn,
    getLegalMoves,
    hashGame,
) where

import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set

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
    let plainGame = Game newPlayer newPlayer Plr1sTurn Set.empty
     in plainGame{history = Set.singleton $ hashGame plainGame}

data GameState = Plr1sTurn | Plr2sTurn | Plr1Won | Plr2Won | Draw deriving (Eq)

type PositionHash = Int

data Game = Game
    { player1 :: Player
    , player2 :: Player
    , gameState :: GameState
    , history :: Set PositionHash
    }

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

hashGame :: Game -> PositionHash
hashGame Game{player1 = plr1, player2 = plr2, gameState = state} =
    stateNum + playersHash
  where
    stateNum =
        ( case state of
            Plr1sTurn -> 1
            Plr2sTurn -> 2
            Plr1Won -> 3
            Plr2Won -> 4
            Draw -> 5
        )
            * 10000
    playersHash =
        a plr1 * 1000
            + b plr1 * 100
            + a plr2 * 10
            + b plr2

checkVictoryOr :: GameState -> Game -> GameState
checkVictoryOr orState g@Game{player1 = plr1, player2 = plr2}
    | a plr1 + b plr1 == 0 = Plr2Won
    | a plr2 + b plr2 == 0 = Plr1Won
    | Set.member (hashGame g) (history g) = Draw
    | otherwise = orState -- TODO: check for Draw!

data TurnAction
    = AToA
    | AToB
    | BToA
    | BToB
    | Split
    deriving (Show)

{- | find out whether the action is legal
for example if going from `a to a` then check whether both players have a hands
-}
isLegal :: Game -> TurnAction -> Bool
isLegal Game{gameState = Draw} _ =
    False
isLegal Game{player1 = plr1, player2 = plr2} AToA =
    not (a plr1 == 0 || a plr2 == 0)
isLegal Game{player1 = plr1, player2 = plr2} BToB =
    not (b plr1 == 0 || b plr2 == 0)
isLegal Game{player1 = plr1, player2 = plr2, gameState = Plr1sTurn} AToB =
    not (a plr1 == 0 || b plr2 == 0)
isLegal Game{player1 = plr1, player2 = plr2, gameState = Plr2sTurn} AToB =
    not (b plr1 == 0 || a plr2 == 0)
isLegal Game{player1 = plr1, player2 = plr2, gameState = Plr1sTurn} BToA =
    not (b plr1 == 0 || a plr2 == 0)
isLegal Game{player1 = plr1, player2 = plr2, gameState = Plr2sTurn} BToA =
    not (a plr1 == 0 || b plr2 == 0)
isLegal Game{player1 = plr1, gameState = Plr1sTurn} Split =
    ((a1 == 0) /= (b1 == 0)) && even (a1 + b1)
  where
    a1 = a plr1
    b1 = b plr1
isLegal Game{player2 = plr2, gameState = Plr2sTurn} Split =
    ((a2 == 0) /= (b2 == 0)) && even (a2 + b2)
  where
    a2 = a plr2
    b2 = b plr2
isLegal _ _ =
    False

{- | assumes action is legal
TODO: make readable with case?
-}
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

playTurn :: Game -> TurnAction -> Game
playTurn g@Game{gameState = state} action =
    case state of
        Plr1sTurn -> doTheTurn Plr2sTurn
        Plr2sTurn -> doTheTurn Plr1sTurn
        _ ->
            g
  where
    doTheTurn otherPlr =
        if isLegal g action
            then
                let movedG = makeMove action g
                    turnedG = movedG{gameState = checkVictoryOr otherPlr movedG}
                    turnedGHash = hashGame turnedG
                 in turnedG{history = Set.insert turnedGHash (history turnedG)}
            else g

getLegalMoves :: Game -> [TurnAction]
getLegalMoves g =
    List.filter (isLegal g) [AToA, AToB, BToA, BToB, Split]
