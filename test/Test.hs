module Main where

import Replay
import Control.Monad.State.Lazy
import Data.Char
import System.Exit
import Test.QuickCheck hiding (Result)

-- | Runs the test suite for the replay library
main :: IO ()
main = verboseCheck checkTestCase

-- | Programs are parameterised over a 'tick' action.
--   Questions are () and answers are integers.
type Program = ReplayT (State Int) () Int Int

tick :: State Int ()
tick = get >>= \n -> put (n + 1)

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
    where show tc = testName tc ++ " " ++ show (testInput tc) ++ 
                    " " ++ show (testResult tc)

-- | Running a program.
runProgram :: Program -> Input -> State Int (Int, Int)
runProgram p inp =
    do
      x <- play p emptyTrace inp
      n <- get
      return (x, n)
    where
      play prog t inp =
          do
            r <- run prog t
            case r of
              Right x      -> return x
              Left (_, t') -> case inp of
                                []       -> error "too few inputs"
                                a : inp' -> play prog (addAnswer a t') inp'

-- | Checking a test case. Compares expected and actual results.
checkTestCase :: TestCase -> Bool
checkTestCase (TestCase name i r p) =
    r == evalState (runProgram p i) 0

-- | List of interesting test cases.
testCases :: [TestCase]
testCases = [
     TestCase
    { testName    = "basic_test1"
    , testInput   = [3,4]
    , testResult  = (8, 1)
    , testProgram = do
        liftR tick
        a <- ask () -- should be 3
        b <- liftR (return 1)
        c <- ask () -- should be 4
        return (a + b + c)
    }
   , TestCase
    { testName    = "basic_test2"
    , testInput   = [0,0]
    , testResult  = (0, 2)
    , testProgram = do
        liftR tick
        a <- ask () -- should be 0
        b <- liftR (return 0)
        c <- ask () -- should be 0
        liftR tick
        return (a + b + c)
    }
   , TestCase
   { testName    = "cut_test1"
   , testInput   = [0,0]
   , testResult  = (0, 0)
   , testProgram = cut (return 0)
   }
  ]

-- | Running all the test cases.
runTests :: [TestCase] -> Bool
runTests = and . map checkTestCase

data MonadElem = Tick | Return Int | Ask Int
                 deriving (Eq)

instance Arbitrary MonadElem where 
    arbitrary = do
      n <- arbitrary
      elements [ Tick , Return n , Ask n ]
                             
genTestCase :: Gen TestCase
genTestCase =
    sized $ \n -> do
      l <- vectorOf n arbitrary :: Gen [MonadElem]
      let -- expected number of ticks
          nTicks = length [ x | x <- l, x == Tick ]
          -- expected result of the program
          s = sum (map (\x -> case x of Tick -> 0
                                        Return n -> n
                                        Ask n -> n) l)
          -- input list fed to the program
          testInp = concatMap (\x -> case x of Tick -> []
                                               Return n -> []
                                               Ask n -> [n]) l
          -- generate the program to be test
          testProg = do res <- sequence (map toMonad l)
                        return $ sum res
               
          testRes = (s, nTicks)    
          testNam = "test" ++ show nTicks
      return $ TestCase testNam testInp testRes testProg
          where
            toMonad (Tick)     = liftM (const 0) (liftR tick)
            toMonad (Return n) = liftR (return n)
            toMonad (Ask n)    = ask ()

instance Arbitrary TestCase where
    arbitrary = genTestCase
