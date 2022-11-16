{-# LANGUAGE InstanceSigs #-}
module Lib where

import Data.Matrix
import Data.Maybe
import System.Random

-- Constants
width = 20
height = 20
pMark = 1
cpuMark = -1

-- | @Direction@ represents the possible course a player can move
data Direction
  = North
  | South
  | East
  | West
  deriving (Show, Eq)

-- | @Move@ represents the possible moves a user can take
data Move
  = MoveLeft
  | MoveRight
  | MoveForward
  deriving (Show, Eq)

-- | @Position@ is a type synonym for the tuple (Int,Int)
-- The first element is the row (y coordinate), second element is the column (x coordinate)
type Position = (Int, Int)

-- | @Turn@ denotes whose turn it is
data Turn = P | CPU
  deriving (Eq, Show)

-- | @Player@ represents a player moving in a specified direction who is at the current position
data Player = Player Direction Position
  deriving (Eq, Show)

-- | @Difficulty@ represents the difficulty of the CPU. Higher levels of difficulty
-- causes the CPU to make more informed decisions on where to move
data Difficulty = Beginner | Medium
  deriving (Eq, Show)

-- | @TronState@ is the heart of the game Tron
-- Single data constructor that takes in the following values:
-- a Matrix of Int's representing the current grid/state of the game
-- the current player with their direction and current position
-- a CPU Player
-- the current turn, either you or the CPU
-- the level of difficulty of the CPU
data TronState = TronState (Matrix Int) Player Player Turn Difficulty
  deriving Eq

-- Customize TronState instance of Show so can print information on new lines
instance Show TronState where
  show :: TronState -> String
  show (TronState matrix p cpu turn difficulty) =
    prettyMatrix matrix ++ "\n"
    ++ show p ++ "\n"
    ++ show cpu ++ "\n"
    ++ "Turn: " ++ show turn ++ " Difficulty: " ++ show difficulty

initTronState :: TronState
initTronState = createTronState height width

-- | @createTronState h w@ initializes a matrix of size @h@ and @w@
-- Initializes the tron state with one player and a cpu, the player goes first
createTronState :: Int -> Int -> TronState
createTronState h w = TronState matrix (Player East playerPos) (Player West cpuPlayerPos) P Beginner
    where
        playerPos = (div h 2, 3) -- remember Position is (row, col) AKA (y, x)
        cpuPlayerPos = (div w 2, w - 2)
        matrix = setElem cpuMark cpuPlayerPos (setElem pMark playerPos (zero h w))

-- | @changeDirection direction move@ evaluates to the next direction state depending on what @move@ is
-- For example, if we are travelling @North@ and we @MoveLeft@, then the next direction would be @East@
changeDirection :: Direction -> Move -> Direction
changeDirection North MoveLeft = West
changeDirection South MoveLeft = East
changeDirection East MoveLeft = North
changeDirection West MoveLeft = South
changeDirection North MoveRight = East
changeDirection South MoveRight = West
changeDirection East MoveRight = South
changeDirection West MoveRight = North
changeDirection d MoveForward = d

-- | @calculateNextPosition direction position@ calculates the next position based on the specified direction
-- ASSUME that (1,1) corresponds to the top left position of the matrix
calculateNextPosition :: Direction -> Position -> Position
calculateNextPosition North (row,col) = (row-1, col)
calculateNextPosition South (row,col) = (row+1, col)
calculateNextPosition East  (row,col) = (row, col+1)
calculateNextPosition West  (row,col) = (row, col-1)

-- | @isOutOfBounds tronState@ checks whether the player is off the grid based off the current players turn
isOutOfBounds :: TronState -> Bool
isOutOfBounds (TronState m p cpu turn _) = isNothing (safeGet row col m)
  where (Player dir (row, col)) = if turn == CPU then cpu else p

-- | @willCollideWithJetTrail tronState move@ checks for any future collisions
-- produces True if the player's current position and move will collide with any jet trail, False otherwise
-- ASSUME the player's new position after moving is not out of bounds
willCollideWithJetTrail :: TronState -> Move -> Bool
willCollideWithJetTrail (TronState m p cpu turn _) move = fromMaybe 1 (safeGet row' col' m) /= 0
  where currP = if turn == CPU then cpu else p
        dir' = changeDirection (getPlayerDirection currP) move
        (row', col') = calculateNextPosition dir' (getPlayerPosition currP)

updatePlayerDirection :: Player -> Move -> Player
updatePlayerDirection (Player d pos) move = Player (changeDirection d move) pos

getPlayerDirection :: Player -> Direction
getPlayerDirection (Player d _) = d

getPlayerPosition :: Player -> Position
getPlayerPosition (Player _ p) = p

-- $moveLeft tronState@ will change direction and then call moveForward with the updated direction
moveLeft :: TronState -> TronState
moveLeft (TronState m p cpu P d) = moveForward (TronState m (updatePlayerDirection p MoveLeft) cpu P d)
moveLeft (TronState m p cpu CPU d) = moveForward (TronState m p (updatePlayerDirection cpu MoveLeft ) CPU d)

-- @moveRight tronState@ moves the player right and forward
moveRight :: TronState -> TronState
moveRight (TronState m p cpu P d) = moveForward (TronState m (updatePlayerDirection p MoveRight) cpu P d)
moveRight (TronState m p cpu CPU d) = moveForward (TronState m p (updatePlayerDirection cpu MoveRight ) CPU d)

-- @moveForward tronState@
-- 1. moves the current player forward in their direction
-- 2. updates the matrix
-- 3. updates the player turn
-- Will not throw an error if moving out of bounds, instead do NOT update matrix. Out of bounds logic will check for this
moveForward :: TronState -> TronState
moveForward (TronState matrix (Player dir pos) cpu P d) = TronState newMatrix (Player dir newPos) cpu CPU d
  where
      newPos = calculateNextPosition dir pos
      newMatrix = fromMaybe matrix (safeSet pMark newPos matrix)
moveForward (TronState matrix p (Player dir pos) CPU d) = TronState newMatrix p (Player dir newPos) P d
  where
      newPos = calculateNextPosition dir pos
      newMatrix = fromMaybe matrix (safeSet cpuMark newPos matrix)

-- | @nextTronStateForPlayer tronState move@ produces the next state of the game based on the current player's move
-- ASSUME @tronstate@ is valid (that is there are no collisions)
-- After we get the "potential" next game state, we need to check for any collisions
nextGameState :: TronState -> Move -> Maybe TronState
nextGameState ts move = if isValidGameState then Just nextTs else Nothing
  where nextTs = case move of
                    MoveLeft -> moveLeft ts
                    MoveRight -> moveRight ts
                    MoveForward -> moveForward ts
        -- IMPORTANT: collision check needs to look at old tron state
        isValidGameState = not (isOutOfBounds nextTs) && not (willCollideWithJetTrail ts move)

getTurn :: TronState -> Turn
getTurn (TronState _ _ _ t _) = t

changeDifficulty :: TronState -> Difficulty -> TronState
changeDifficulty (TronState m p cpu t _) = TronState m p cpu t

getDifficulty :: TronState -> Difficulty
getDifficulty (TronState _ _ _ _ d) = d

-- | @advanceCPUStateIO tronState@ calculates the next position of the CPU
-- CPU's can have varying difficulties, which allow them to make informed decisions on which move to pick next
advanceCPUStateIO :: TronState -> IO (Maybe TronState)
advanceCPUStateIO ts = do
    let difficulty = getDifficulty ts
    cpuMove <- case difficulty of
      Beginner -> return (beginnerCPUAlgorithm ts)
      Medium -> do
        mediumCPUAlgorithm ts <$> (randomRIO (0, 100) :: IO Int)
    return (nextGameState ts cpuMove)

-- | @beginnerCPUAlgorithm ts@ is a very simple CPU, it plays to SURVIVE
-- It predicts one step into the future, either moving forward, right or left.
-- If any of those moves does not result in a loss, we choose one (priority goes towards forward, left, then right)
beginnerCPUAlgorithm :: TronState -> Move
beginnerCPUAlgorithm ts@(TronState m _ cpu _ _) = move
  where
    nextTsForward = if isNothing (nextGameState ts MoveForward) then Nothing else Just MoveForward
    nextTsRight = if isNothing (nextGameState ts MoveRight) then Nothing else Just MoveRight
    nextTsLeft = if isNothing (nextGameState ts MoveLeft) then Nothing else Just MoveLeft
    nextStates = [nextTsForward, nextTsLeft, nextTsRight]
    validMoves = catMaybes nextStates
    move = if null validMoves then MoveRight else head validMoves

-- | @mediumCPUAlgorithm ts r@ chooses where to move given a random number between 0 and 100 inclusive.
-- However, it is similar to @beginnerCPUAlgorithm@ in the sense that it will override the chosen move with
-- a new move, if it results in a loss. Therefore, this introduces some unpredictability in where the
-- CPU can move
mediumCPUAlgorithm :: TronState -> Int -> Move
mediumCPUAlgorithm ts r
  | r <= lo = if canMoveRight then MoveRight else if canMoveForward then MoveForward else MoveLeft
  | r >= hi = if canMoveLeft then MoveLeft else if canMoveForward then MoveForward else MoveRight
  -- r is between (lo, hi) non-inclusive
  | otherwise = if canMoveForward then MoveForward else
                  if r <= mid && canMoveLeft then MoveLeft else MoveRight
  where
    canMoveRight = isJust (nextGameState ts MoveRight)
    canMoveLeft = isJust (nextGameState ts MoveLeft)
    canMoveForward = isJust (nextGameState ts MoveForward)
    lo = 10
    hi = 90
    mid = (hi-lo) `div` 2

-- | @advanceCPUState tronState@ calculates the next position of the CPU
-- DEPRECATED - used only for proposal, see advanceCPUStateIO for connction with Gloss
advanceCPUState :: TronState -> Maybe TronState
advanceCPUState ts = nextGameState ts cpuMove
  where difficulty = getDifficulty ts
        cpuMove = case difficulty of
                    Beginner -> beginnerCPUAlgorithm ts
                    Medium -> mediumCPUAlgorithm ts 50 -- always MoveForward

-- | @printNextGameState tronState@
-- Our proof of concept demo will use this function which repeatedly asks you each time whether to move right, left, or forward
-- Will print the next valid game state, otherwise if you've crashed then the game is over
printNextGameState :: TronState -> IO()
printNextGameState ts =
  putStrLn "What's your next move? Choose from [Right, Left, Forward]"
  >> getLine
  >>= readMove
  >>= handleNextTronState ts

readMove :: String -> IO Move
readMove "Right" = return MoveRight
readMove "Left" = return MoveLeft
readMove "Forward" = return MoveForward
readMove _ = undefined

handleNextTronState :: TronState -> Move -> IO()
handleNextTronState ts move =
  case turn of
    CPU -> printCPUState
    P -> printPState
  where
    turn = getTurn ts
    getAction (Player dir pos) = " moved " ++ show dir ++ " and are now at position " ++ show pos
    printCPUState = case advanceCPUState ts of
                      Nothing -> putStrLn "You win!"
                      Just ts'@(TronState m _ cpu' _ _) -> putStrLn (prettyMatrix m)
                        >> putStrLn ("CPU" ++ getAction cpu')
                        >> printNextGameState ts'
    printPState = case nextGameState ts move of
                    Nothing -> putStrLn "You lost!"
                    Just ts'@(TronState m p' _ _ _) -> putStrLn (prettyMatrix m)
                        >> putStrLn ("You" ++ getAction p')
                        >> putStrLn "Now it's the CPU's turn..."
                        >> handleNextTronState ts' move
