module UI where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe
import Data.Matrix
import Data.List
import System.Exit
import System.Directory
import Data.Function
import Control.Monad
import Text.Read (readMaybe)

-- Constants
winWidth = 700
gridSize = 71
boxWidth = winWidth `div` (gridSize - 1)
window = InWindow "Tron" (winWidth+50, winWidth+50) (winWidth `div` 2, 0)
backgroundColor = black
playerColor = blue
cpuColor = red
scoresFile = "src/scores.txt"

-- | @Mode@ describes the current mode of the game.
-- For example, the game can be over, in progress, or the user is in the menu, etc.
data Mode = Win
    | Loss
    | InProgress
    | Menu
    | ViewScores
    deriving Eq

-- | @GameState@ is a wrapper around @TronState@ that holds GUI specified metadata, not information
-- that is CORE to the game.
-- Contains additional information like what Move the user selected, current @Mode@ of the game,
-- the metadata of all the scores and the current score.
type GameState = (TronState, Move, Mode, Int)

-- | @startingGameState@ starts the game in the Menu
startingGameState :: IO GameState
startingGameState = return (createTronState gridSize gridSize, MoveForward, Menu, 0)

-- | @retryGameState oldGameState@ starts the game all over again, but InProgress and with
-- the difficulty from @oldGameState@
retryGameState :: GameState -> IO GameState
retryGameState (oldTs, _, _, _) = return (newTs, MoveForward, InProgress, 0)
    where
        difficulty = getDifficulty oldTs
        newTs = changeDifficulty (createTronState gridSize gridSize) difficulty
    

-- | @start@ plays a game in a window, using IO actions to build the pictures
start :: IO ()
start = do
    initState <- startingGameState
    playIO
        window
        backgroundColor
        8
        initState
        drawGameState
        handleEvent
        handleStep

-- | @getScores@ gets the scores from @scoresFile@ and takes the top 10 highest scores
getScores :: IO [Int ]
getScores = do
    createFileIfNotExists
    contents <- readFile scoresFile
    return ((take 10 . sortNumeric . filter (not . null) . lines) contents)
    where
        sortNumeric =  reverse . sort . mapMaybe (readMaybe :: String -> Maybe Int)
        createFileIfNotExists = do
            doesExist <- doesFileExist scoresFile
            unless doesExist $ writeFile scoresFile ""

-- | @addScore score@ adds a score to the file pointed by @scoresFile@
addScore :: Int -> IO ()
addScore score = appendFile scoresFile scoreText
    where scoreText = "\n" ++ show score

-- | @drawGameState gs@ draws the game state depending on the curernt @Mode@ of the game
drawGameState :: GameState -> IO Picture
drawGameState (TronState m _ _ _ _, _, InProgress, score) = return (drawGrid m score)
drawGameState (_, _, Menu, _) = return drawMenu
drawGameState (_, _, ViewScores, _) = drawScores
drawGameState (_, _, m, score) = return (drawGameOver m score)

-- | @drawMenu@ gives the starting menu where the player can select a difficulty or leave the game
drawMenu :: Picture
drawMenu = pictures [
    color blue (translate (-35) 20 (scale 0.2 0.2 (text "TRON"))),
    color yellow (translate (-180) (-30) (scale 0.1 0.1 (text "Select difficulty to start: [1] Easy, [2] Medium, [3] Hard"))),
    color yellow (translate (-60) (-80) (scale 0.1 0.1 (text "Click v to view scores"))),
    color yellow (translate (-60) (-130) (scale 0.1 0.1 (text "Press esc to exit")))]

-- | @drawScores@ reads from the scores file, sorts and takes the top 10, and lastly displays them onto the screen
-- the user is allowed to clear the scores, or head back to the menu
drawScores :: IO Picture
drawScores = do
        scores <- getScores
        return (pictures ([scoreImg, clearScoreImg, menuImg] ++ scoreImages scores))
    where
        scoreImages los = if null los then [emptyScoreImg] else mapInd scorePicture los
        emptyScoreImg = color yellow (translate (-60) 150 (scale 0.1 0.1 (text "No scores yet!")))
        scorePicture score index = color yellow (translate 0 (fromIntegral (index-6)*(-25)) (scale 0.1 0.1 (text (show score))))
        menuImg = color yellow (translate (-60) (-250) (scale 0.1 0.1 (text "Press m for menu")))
        clearScoreImg = color yellow (translate (-60) (-200) (scale 0.1 0.1 (text "Press c to clear scores")))
        scoreImg = color yellow (translate (-120) 200 (scale 0.2 0.2 (text "Your top 10 scores")))

-- variant of map that passes each element's index as a second argument to f
mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

-- | @drawGameOver@ tells whether the player won or lost and gives them the option to retry, go to the menu, or leave the game
drawGameOver :: Mode -> Int -> Picture
drawGameOver m score = pictures [img, tryAgainImg, menuImg, exitImg, scoreImage]
    where
        winImage = color blue (translate (-60) 50 (scale 0.2 0.2 (text "You won!")))
        lossImage = color red (translate (-60) 50 (scale 0.2 0.2 (text "You lost!")))
        img = if m == Win then winImage else lossImage
        scoreText = "You got a score of: " ++ show score
        scoreImage = color yellow (translate (-100) 0 (scale 0.1 0.1 (text scoreText)))
        tryAgainImg = color yellow (translate (-60) (-50) (scale 0.1 0.1 (text "Press r to retry")))
        menuImg = color yellow (translate (-60) (-100) (scale 0.1 0.1 (text "Press m for menu")))
        exitImg = color yellow (translate (-60) (-150) (scale 0.1 0.1 (text "Press esc to exit")))

-- | @drawGrid matrix@ draws the current grid layout of the game
-- 0 means the cell is unoccupied
-- If entry in matrix is 1 or -1, we color the cell
drawGrid :: Matrix Int -> Int -> Picture
drawGrid m score = pictures ([gridImg, scoreImg] ++ pics)
    where
        pics = map getPic (filter nonZero [ (row, col, getElem row col m) | row <- [1..gridSize], col <- [1..gridSize]])
        nonZero (row, col, elem) = elem /= 0
        getPic (row, col, elem) = drawBox (row, col) elem
        gridImg = color white (rectangleWire (fromIntegral winWidth + 10) (fromIntegral winWidth + 10))
        scoreImg =  color yellow (translate 0 360 (scale 0.1 0.1 (text (show score))))

-- | @drawBox pos val@ draws a rectangle at the specified pos, and colors the cell
-- depending on whether it's the cpu or player's cell
drawBox :: (Int, Int) -> Int -> Picture
drawBox _ 0 = Blank
drawBox (row, col) val
    | val == -1 = square cpuColor
    | otherwise = square playerColor
    where
        square c = color c (translate x' y' (rectangleSolid s s))
        s = fromIntegral boxWidth
        halfWidth = fromIntegral(winWidth `div` 2)
        -- since matrix is 1 based indexing, and gloss starts (0,0) at center
        x' = s * fromIntegral (col-1) - halfWidth
        y' = s * fromIntegral (row-1) - halfWidth

-- | @handleKey key gamestate@ listens to key events depending on the mode of the game
handleKey :: Key -> GameState -> IO GameState
-- game is currently in progress, listen to what arrow keys pressed
handleKey key gs@(ts, _, InProgress, score) =
    case turn of
        CPU -> return gs
        P -> return (ts, move', InProgress, score)
    where
        turn = getTurn ts
        (TronState _ p _ _ _) = ts
        dir = getPlayerDirection p
        move' = newMove dir key
-- user in Menu, allow them to choose CPU difficulty, view scores, or exit
handleKey key gs@(ts, move, Menu, _) = case key of
    Char '1' -> return (changeDifficulty ts Beginner, move, InProgress, 0)
    Char '2' -> return (changeDifficulty ts Medium, move, InProgress, 0)
    Char '3' -> return (changeDifficulty ts Beginner, move, InProgress, 0) -- TODO change once we add CPU difficulty
    Char 'v' -> return (ts, move, ViewScores, 0)
    SpecialKey KeyEsc -> do
        exitSuccess
        return gs
    _ -> return gs
-- user viewing scores, let them go to menu or clear scores
handleKey key gs@(ts, move, ViewScores, _) = case key of
    Char 'm' -> startingGameState
    Char 'c' -> do
        writeFile scoresFile ""
        return gs
    _ -> return gs
-- guaranteed that now game is either win or loss, let user retry, go to menu, or exit
handleKey key gs@(_, _, _, score) = case key of
    Char 'r' -> do
        addScore score
        retryGameState gs
    Char 'm' -> do
        addScore score
        startingGameState
    SpecialKey KeyEsc -> do
        exitSuccess
        return gs
    _ -> return gs

-- | @newMove direction key@ calculates the new Move based on the key pressed and 
-- the current direction the player was travelling. We do NOT allow a user to go backwards
newMove :: Direction -> Key -> Move
newMove dir key = case dir of
    North -> case key of
        SpecialKey KeyRight -> MoveRight
        SpecialKey KeyLeft -> MoveLeft
        _ -> MoveForward
    South -> case key of
        SpecialKey KeyRight -> MoveLeft
        SpecialKey KeyLeft -> MoveRight
        _ -> MoveForward
    East -> case key of
        SpecialKey KeyUp -> MoveRight
        SpecialKey KeyDown -> MoveLeft
        _ -> MoveForward
    West  -> case key of
        SpecialKey KeyUp -> MoveLeft
        SpecialKey KeyDown -> MoveRight
        _ -> MoveForward

-- | @handleEvent@ handles any event in the game, for now we only care about key events
handleEvent :: Event -> GameState -> IO GameState
handleEvent event gs = case event of
    EventKey key _ _ _ -> handleKey key gs
    _ -> return gs

-- | @handleStep time gamestate@ advances the state of the game depending on whose turn it is
handleStep :: Float -> GameState -> IO GameState
handleStep _ (ts, move, InProgress, score) = case turn of
        CPU -> do
            nextCPUState <- advanceCPUStateIO ts
            return (case nextCPUState of
                Nothing -> (ts, move, Win, score+500)
                Just ts' -> (ts', MoveForward, InProgress, newScore)) -- reset to move forward
        P -> case nextGameState ts move of
                Nothing -> return (ts, move, Loss, score)
                Just ts' -> return (ts', MoveForward , InProgress, newScore) -- reset to move forward
    where
        turn = getTurn ts
        newScore = score + 100
handleStep _ gs = return gs
