import Lib
import Test.Tasty
import Test.Tasty.HUnit
import Data.Matrix

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
  initStateTests,
  changeDirectionTests,
  calculateNextPositionTests,
  jetTrailCollisionTests,
  moveFowardTests,
  moveRightTests,
  moveLeftTests,
  isOutOfBoundsTests]

mockPlayer = Player West (-100,-100)

-- main = defaultMain $ testGroup "Tests" $
--   [ testCase "Addition works" $ do
--       2 + 3 @?= 5  -- actual @?= expected
--   , testCase "Multiplication works" $ do
--       6 @=? 2 * 3  -- expected @=? actual
--   -- NOTE: Uncomment this to see what a failing assertion looks like:
--   -- , testCase "Bad assertion" $ do
--   --     1 @?= 2
--   -- NOTE: This is how to explicitly assert failures:
--   -- , testCase "Explicit failure" $ do
--   --     assertFailure "BOOM!"
--   ]

initStateTests = testGroup "initState unit tests"
  [
    testCase "initital tron state" $
      initTronState  @?= TronState
      (setElem cpuMark (div height 2, width - 2) (setElem pMark (div height 2, 3) (zero height width)))
      (Player East (div height 2, 3))
      (Player West (div height 2, width - 2))
      P
      Beginner 
  ]

changeDirectionTests = testGroup "changeDirection unit tests"
  [ testCase "North MoveLeft" $
      changeDirection North MoveLeft @?= West

    , testCase "South MoveLeft" $
      changeDirection South MoveLeft @?= East

    , testCase "East MoveLeft" $
      changeDirection East MoveLeft @?= North

    , testCase "West MoveLeft" $
      changeDirection West MoveLeft @?= South

    , testCase "North MoveRight" $
      changeDirection North MoveRight @?= East

    , testCase "South MoveRight" $
      changeDirection South MoveRight @?= West

    , testCase "East MoveRight" $
      changeDirection East MoveRight @?= South

    , testCase "West MoveRight" $
      changeDirection West MoveRight @?= North

    , testCase "North MoveForward" $
      changeDirection North MoveForward @?= North

    , testCase "South MoveForward" $
      changeDirection South MoveForward @?= South

    , testCase "East MoveForward" $
      changeDirection East MoveForward @?= East

    , testCase "West MoveForward" $
      changeDirection West MoveForward @?= West
  ]

calculateNextPositionTests = testGroup "calculateNextPosition unit tests"
  [ testCase "calculateNextPosition North" $
      calculateNextPosition North (10, 10) @?= (9, 10)

    , testCase "calculateNextPosition South" $
      calculateNextPosition South (10, 10) @?= (11, 10)

    , testCase "calculateNextPosition East" $
      calculateNextPosition East (10, 10) @?= (10, 11)

    , testCase "calculateNextPosition West" $
      calculateNextPosition West (10, 10) @?= (10, 9)
  ]

jetTrailCollisionTests = testGroup "jetTrailCollision unit tests"
  [
    testCase "no collision moving forward on initial state" $
      willCollideWithJetTrail initTronState MoveForward @?= False

    , testCase "no collision moving right on initial state" $
      willCollideWithJetTrail initTronState MoveLeft @?= False

    , testCase "no collision moving left on initial state" $
      willCollideWithJetTrail initTronState MoveRight @?= False

    -- starting at (2,2) South, MoveForward should not collide
    -- ┌       ┐
    -- │ 1 1 1 │
    -- │ 1 0 1 │
    -- │ 1 0 1 │
    -- │ 1 1 1 │
    -- └       ┘
    , testCase "no collision moving forward surrounded by jetrails" $
      willCollideWithJetTrail
        (TronState (fromLists [[1,1,1], [1,0,1], [1,0,1], [1,1,1]]) (Player South (2,2)) mockPlayer P Beginner)
        MoveForward @?= False

    -- starting at (2,2) North, MoveRight should not collide
    -- ┌       ┐
    -- │ 1 1 1 │
    -- │ 1 0 0 │
    -- │ 1 1 1 │
    -- └       ┘
    , testCase "no collision moving right surrounded by jetrails" $
      willCollideWithJetTrail
        (TronState (fromLists [[1,1,1], [1,0,0], [1,1,1]]) (Player North (2,2)) mockPlayer P Beginner)
        MoveRight @?= False

    -- starting at (2,2) North, MoveLeft should not collide
    -- ┌       ┐
    -- │ 1 1 1 │
    -- │ 0 0 1 │
    -- │ 1 1 1 │
    -- └       ┘
    , testCase "no collision moving left surrounded by jetrails" $
      willCollideWithJetTrail
        (TronState (fromLists [[1,1,1], [0,0,1], [1,1,1]]) (Player North (2,2)) mockPlayer P Beginner)
        MoveLeft @?= False

    -- starting at (2,2) North, MoveRight should not collide
    -- ┌       ┐
    -- │ 1 1 1 │
    -- │ 1 0 0 │
    -- │ 1 1 1 │
    -- └       ┘
    , testCase "no collision moving right surrounded by jetrails" $
      willCollideWithJetTrail
        (TronState (fromLists [[1,1,1], [1,0,0], [1,1,1]]) (Player North (2,2)) mockPlayer P Beginner)
        MoveRight @?= False

    -- starting at (2,2) North, MoveForward should collide
    -- ┌       ┐
    -- │ 0 1 0 │
    -- │ 0 0 0 │
    -- │ 0 0 0 │
    -- └       ┘
    , testCase "collision moving forward" $
      willCollideWithJetTrail
        (TronState (fromLists [[0,1,0], [0,0,0], [0,0,0]]) (Player North (2,2)) mockPlayer P Beginner)
        MoveForward @?= True

    -- starting at (2,2) North, MoveRight should collide
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ 0 0 1 │
    -- │ 0 0 0 │
    -- └       ┘
    , testCase "collision moving right" $
      willCollideWithJetTrail
        (TronState (fromLists [[0,0,0], [0,0,1], [0,0,0]]) (Player North (2,2)) mockPlayer P Beginner)
        MoveRight @?= True

    -- starting at (2,2) North, MoveLeft should collide
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ 1 0 0 │
    -- │ 0 0 0 │
    -- └       ┘
    , testCase "collision moving left" $
      willCollideWithJetTrail
        (TronState (fromLists [[0,0,0], [1,0,0], [0,0,0]]) (Player North (2,2)) mockPlayer P Beginner)
        MoveLeft @?= True

    -- starting at (2,2) North, MoveLeft should collide
    -- ┌         ┐
    -- │  0 0 0  │
    -- │ -1 0 0  │
    -- │  0 0 0  │
    -- └         ┘
    , testCase "collision moving left into CPU trail" $
      willCollideWithJetTrail
        (TronState (fromLists [[0,0,0], [-1,0,0], [0,0,0]]) (Player North (2,2)) mockPlayer P Beginner)
        MoveLeft @?= True

    -- starting at (2,2) North, MoveLeft should collide
    -- ┌         ┐
    -- │  0 0 0  │
    -- │ -1 0 0  │
    -- │  0 0 0  │
    -- └         ┘
    , testCase "CPU - collision moving left into own trail" $
      willCollideWithJetTrail
        (TronState (fromLists [[0,0,0], [-1,0,0], [0,0,0]]) mockPlayer (Player North (2,2)) CPU Beginner)
        MoveLeft @?= True

    -- starting at (2,2) North, MoveLeft should collide
    -- ┌         ┐
    -- │  0 0 0  │
    -- │  1 0 0  │
    -- │  0 0 0  │
    -- └         ┘
    , testCase "CPU - collision moving left into player trail" $
      willCollideWithJetTrail
        (TronState (fromLists [[0,0,0], [-1,0,0], [0,0,0]]) mockPlayer (Player North (2,2)) CPU Beginner)
        MoveLeft @?= True

    , testCase "CPU - no collision moving forward surrounded by jetrails" $
      willCollideWithJetTrail
        (TronState (fromLists [[1,1,1], [-1,0,-1], [-1,0,-1], [-1,1,-1]]) mockPlayer (Player South (2,2)) CPU Beginner)
        MoveForward @?= False
  ]

isOutOfBoundsTests = testGroup "isOutOfBounds unit tests"
  [
    testCase "inital state should not be out of bounds" $
      isOutOfBounds initTronState @?= False

    -- ┌       ┐
    -- │ 1 1 1 │
    -- │ 1 0 1 │
    -- │ 1 1 1 │
    -- └       ┘
    , testCase "should not be out of bounds if within matrix" $
      isOutOfBounds (TronState (fromLists [[1,1,1], [1,0,1], [1,1,1]]) (Player South (2,2)) mockPlayer P Beginner)
         @?= False

    , testCase "Should detect if TOP out of bounds" $
      isOutOfBounds (TronState (fromLists [[1,1,1], [1,1,1], [1,1,1]]) (Player South (0,2)) mockPlayer P Beginner)
         @?= True

    , testCase "Should detect if BOTTOM out of bounds" $
      isOutOfBounds (TronState (fromLists [[1,1,1], [1,1,1], [1,1,1]]) (Player South (4,2)) mockPlayer P Beginner)
         @?= True

    , testCase "Should detect if LEFT out of bounds" $
      isOutOfBounds (TronState (fromLists [[1,1,1], [1,1,1], [1,1,1]]) (Player South (2,0)) mockPlayer P Beginner)
         @?= True

    , testCase "Should detect if RIGHT out of bounds" $
      isOutOfBounds (TronState (fromLists [[1,1,1], [1,1,1], [1,1,1]]) (Player South (2,4)) mockPlayer P Beginner)
         @?= True
    
    , testCase "CPU - Should detect if RIGHT out of bounds" $
      isOutOfBounds (TronState (fromLists [[1,1,1], [1,1,1], [1,1,1]]) mockPlayer (Player South (2,4)) CPU Beginner)
         @?= True
  ]

moveFowardTests = testGroup "moveFoward unit tests"
  [
    -- starting at (2,2) West, should be able to move forward
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ * 1 0 │ (* is where we expect to be)
    -- │ 0 0 0 │
    -- └       ┘
    testCase "move forward west - player turn" $
      moveForward (TronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) (Player West (2,2)) mockPlayer P Beginner)
      @?= TronState (fromLists [[0,0,0], [1,1,0], [0,0,0]]) (Player West (2,1)) mockPlayer CPU Beginner

    -- starting at (2,2) East, should be able to move forward
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ 0 1 * │ (* is where we expect to be)
    -- │ 0 0 0 │
    -- └       ┘
    ,testCase "move forward east - player turn" $
      moveForward (TronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) (Player East (2,2)) mockPlayer P Beginner)
      @?= TronState (fromLists [[0,0,0], [0,1,1], [0,0,0]]) (Player East (2,3)) mockPlayer CPU Beginner

    -- starting at (2,2) South, should be able to move forward
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ 0 1 0 │ (* is where we expect to be)
    -- │ 0 * 0 │
    -- └       ┘
    ,testCase "move forward south - player turn" $
      moveForward (TronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) (Player South (2,2)) mockPlayer P Beginner)
      @?= TronState (fromLists [[0,0,0], [0,1,0], [0,1,0]]) (Player South (3,2)) mockPlayer CPU Beginner

    -- starting at (2,2) North, should be able to move forward
    -- ┌       ┐
    -- │ 0 * 0 │
    -- │ 0 1 0 │ (* is where we expect to be)
    -- │ 0 0 0 │
    -- └       ┘
    ,testCase "move forward north - player turn" $
      moveForward (TronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) (Player North (2,2)) mockPlayer P Beginner)
      @?= TronState (fromLists [[0,1,0], [0,1,0], [0,0,0]]) (Player North (1,2)) mockPlayer CPU Beginner

    ,testCase "move forward ABOUT to be out of bounds should not fail, instead give old matrix and update players position" $
      moveForward (TronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) (Player West (2,1)) mockPlayer P Beginner)
      @?= TronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) (Player West (2,0)) mockPlayer CPU Beginner

    ,testCase "move forward ALREADY out of bounds should not fail, instead give old matrix and update players position" $
      moveForward (TronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) (Player West (2,0)) mockPlayer P Beginner)
      @?= TronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) (Player West (2,-1)) mockPlayer CPU Beginner

        -- starting at (2,2) West, should be able to move forward
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ * 1 0 │ (* is where we expect to be)
    -- │ 0 0 0 │
    -- └       ┘
    ,testCase "move forward west - cpu turn" $
      moveForward (TronState (fromLists [[0,0,0], [0,-1,0], [0,0,0]]) mockPlayer (Player West (2,2)) CPU Beginner)
      @?= TronState (fromLists [[0,0,0], [-1,-1,0], [0,0,0]]) mockPlayer (Player West (2,1)) P Beginner

    -- starting at (2,2) East, should be able to move forward
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ 0 1 * │ (* is where we expect to be)
    -- │ 0 0 0 │
    -- └       ┘
    ,testCase "move forward east - cpu turn" $
      moveForward (TronState (fromLists [[0,0,0], [0,-1,0], [0,0,0]]) mockPlayer (Player East (2,2)) CPU Beginner)
      @?= TronState (fromLists [[0,0,0], [0,-1,-1], [0,0,0]]) mockPlayer (Player East (2,3)) P Beginner

    -- starting at (2,2) South, should be able to move forward
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ 0 1 0 │ (* is where we expect to be)
    -- │ 0 * 0 │
    -- └       ┘
    ,testCase "move forward south - cpu turn" $
      moveForward (TronState (fromLists [[0,0,0], [0,-1,0], [0,0,0]]) mockPlayer (Player South (2,2)) CPU Beginner)
      @?= TronState (fromLists [[0,0,0], [0,-1,0], [0,-1,0]]) mockPlayer (Player South (3,2)) P Beginner 

    -- starting at (2,2) North, should be able to move forward
    -- ┌       ┐
    -- │ 0 * 0 │
    -- │ 0 1 0 │ (* is where we expect to be)
    -- │ 0 0 0 │
    -- └       ┘
    ,testCase "move forward north - cpu turn" $
      moveForward (TronState (fromLists [[0,0,0], [0,-1,0], [0,0,0]]) mockPlayer (Player North (2,2)) CPU Beginner)
      @?= TronState (fromLists [[0,-1,0], [0,-1,0], [0,0,0]]) mockPlayer (Player North (1,2)) P Beginner
  ]

moveRightTests = testGroup "moveRight unit tests"
  [
    -- starting at (2,2) West, should be able to move right
    -- ┌       ┐
    -- │ 0 * 0 │
    -- │ 0 1 0 │ (* is where we expect to be)
    -- │ 0 0 0 │
    -- └       ┘
    testCase "move right from west should go north" $
      moveRight (TronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) (Player West (2,2)) mockPlayer P Beginner)
      @?= TronState (fromLists [[0,1,0], [0,1,0], [0,0,0]]) (Player North (1,2)) mockPlayer CPU Beginner

    -- starting at (2,2) East, should be able to move right
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ 0 1 0 │ (* is where we expect to be)
    -- │ 0 * 0 │
    -- └       ┘
    ,testCase "move right from east should go south" $
      moveRight (TronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) (Player East (2,2)) mockPlayer P Beginner)
      @?= TronState (fromLists [[0,0,0], [0,1,0], [0,1,0]]) (Player South (3,2)) mockPlayer CPU Beginner

    -- starting at (2,2) South, should be able to move right
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ * 1 0 │ (* is where we expect to be)
    -- │ 0 0 0 │
    -- └       ┘
    ,testCase "move right from south should go west" $
      moveRight (TronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) (Player South (2,2)) mockPlayer P Beginner)
      @?= TronState (fromLists [[0,0,0], [1,1,0], [0,0,0]]) (Player West (2,1)) mockPlayer CPU Beginner

    -- starting at (2,2) North, should be able to move right
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ 0 1 * │ (* is where we expect to be)
    -- │ 0 0 0 │
    -- └       ┘
    ,testCase "move right from north should go east" $
      moveRight (TronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) (Player North (2,2)) mockPlayer P Beginner)
      @?= TronState (fromLists [[0,0,0], [0,1,1], [0,0,0]]) (Player East (2,3)) mockPlayer CPU Beginner

    ,testCase "CPU - move right from north should go east" $
      moveRight (TronState (fromLists [[0,0,0], [0,-1,0], [0,0,0]]) mockPlayer (Player North (2,2)) CPU Beginner)
      @?= TronState (fromLists [[0,0,0], [0,-1,-1], [0,0,0]]) mockPlayer (Player East (2,3)) P Beginner
  ]

moveLeftTests = testGroup "moveLeft unit tests"
  [
    -- starting at (2,2) West, should be able to move left
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ 0 1 0 │ (* is where we expect to be)
    -- │ 0 * 0 │
    -- └       ┘
    testCase "move left from west should go north" $
      moveLeft (TronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) (Player West (2,2)) mockPlayer P Beginner)
      @?= TronState (fromLists [[0,0,0], [0,1,0], [0,1,0]]) (Player South (3,2)) mockPlayer CPU Beginner

    -- starting at (2,2) East, should be able to move left
    -- ┌       ┐
    -- │ 0 * 0 │
    -- │ 0 1 0 │ (* is where we expect to be)
    -- │ 0 0 0 │
    -- └       ┘
    ,testCase "move left from east should go south" $
      moveLeft (TronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) (Player East (2,2)) mockPlayer P Beginner)
      @?= TronState (fromLists [[0,1,0], [0,1,0], [0,0,0]]) (Player North (1,2)) mockPlayer CPU Beginner

    -- starting at (2,2) South, should be able to move left
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ 0 1 1 │ (* is where we expect to be)
    -- │ 0 0 0 │
    -- └       ┘
    ,testCase "move left from south should go west" $
      moveLeft (TronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) (Player South (2,2)) mockPlayer P Beginner)
      @?= TronState (fromLists [[0,0,0], [0,1,1], [0,0,0]]) (Player East (2,3)) mockPlayer CPU Beginner

    -- starting at (2,2) North, should be able to move left
    -- ┌       ┐
    -- │ 0 0 0 │
    -- │ * 1 0 │ (* is where we expect to be)
    -- │ 0 0 0 │
    -- └       ┘
    ,testCase "move left from north should go west" $
      moveLeft (TronState (fromLists [[0,0,0], [0,1,0], [0,0,0]]) (Player North (2,2)) mockPlayer P Beginner)
      @?= TronState (fromLists [[0,0,0], [1,1,0], [0,0,0]]) (Player West (2,1)) mockPlayer CPU Beginner

    ,testCase "CPU - move left from north should go west" $
      moveLeft (TronState (fromLists [[0,0,0], [0,-1,0], [0,0,0]]) mockPlayer (Player North (2,2)) CPU Beginner)
      @?= TronState (fromLists [[0,0,0], [-1,-1,0], [0,0,0]]) mockPlayer (Player West (2,1)) P Beginner
  ]

mockTronState :: Matrix Int -> Direction -> Position -> Turn -> TronState
mockTronState matrix direction pos turn = TronState matrix (Player direction pos) (Player direction pos) turn Beginner 