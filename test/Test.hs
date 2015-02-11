module Main where

import Replay
import Control.Monad.State.Lazy
import Data.IORef
import System.Exit
import Test.QuickCheck hiding (Result)

-- | Runs the test suite for the replay library
main :: IO ()
main = verboseCheck checkTestCase

-- | Programs are parameterised over a 'tick' action.
--   Questions are () and answers are integers.
type Program = State Int () -> ReplayT (State Int) () Int Int

-- | A result is a pair of the final result of the program
--   and the number of 'ticks' executed.
type Result  = (Int, Int)
type Input   = [Int]

-- | A test case collects a program and a list of answers together
--   with its expected result.
data TestCase = TestCase
  { testName    :: String
  , testInput   :: Input
  , testResult  :: Result
  , testProgram :: Program
  }
              
instance Show TestCase
    where show tc = testName tc ++ " " ++ show (testInput tc) ++ " " ++ show (testResult tc)

-- | Running a program.
runProgram :: Program -> Input -> State Int (Int, Int)
runProgram p inp =
    let tick = do { x <- get ; put (x + 1) }
    in do
  -- p :: Program
  -- inp :: Input
  --  put 0
    x <- play (p tick) emptyTrace inp
    n <- get
    return (x, n)
  where
    play prog t inp = do
      r <- run prog t
      case r of
        Right x      -> return x
        Left (_, t') -> case inp of
          []       -> error "too few inputs"
          a : inp' -> play prog (addAnswer t' a) inp'

-- | Checking a test case. Compares expected and actual results.
checkTestCase :: TestCase -> Bool
checkTestCase (TestCase name i r p) =
    r == evalState (runProgram p i) 0

-- | List of interesting test cases.
testCases :: [TestCase]
testCases = [
     TestCase
    { testName    = "test1"
    , testInput   = [3,4]
    , testResult  = (8, 1)
    , testProgram = \tick -> do
        io tick
        a <- ask () -- should be 3
        b <- io (return 1)
        c <- ask () -- should be 4
        return (a + b + c)
    }
   , TestCase
    { testName  = "test2"
    , testInput   = [0,0]
    , testResult  = (0, 2)
    , testProgram = \tick -> do
        io tick
        a <- ask () -- should be 0
        b <- io (return 0)
        c <- ask () -- should be 0
        io tick
        return (a + b + c)
    }
  ]

-- | Running all the test cases.
runTests :: [TestCase] -> Bool
runTests = and . map checkTestCase


genTestCase :: Gen TestCase
genTestCase = do
   testInp  <- listOf arbitrary           :: Gen [Int]
   numTicks <- arbitrary `suchThat` (>0)  :: Gen Int
   let testRes = (sum testInp, numTicks)
       testNam = "test " ++ show (testInp, numTicks)
       testProg = \tick -> sequence_ (replicate numTicks (io tick)) >> return (sum testInp)

{- For simplicity: just one tick combined with returning testInp

       testProg = \tick -> io tick >> return testInp
-}
   return $ TestCase testNam testInp testRes testProg

instance Arbitrary TestCase where
    arbitrary = genTestCase
