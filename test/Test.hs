module Main where

import Replay
import Control.Monad.State.Lazy
import Data.Char
import System.Exit
import Test.QuickCheck hiding (Result)

-- | Runs the test suite for the replay library
main :: IO ()
main = do mapM_ runTest testCases 
          quickCheck checkTestCase

-- | Run a handmade testcase
runTest :: TestCase -> IO ()
runTest tc = do putStr $ testName tc ++ ": "
                print $ checkTestCase tc

--------------------------------------------------------------------------------

-- | Programs are parameterised over a 'tick' action.
--   Questions are () and answers are integers.
type Program = ReplayT (State Int) () Int Int

-- | A result is a pair of the final result of the program
--   and the number of 'ticks' executed.
type Result  = (Int, Int)
type Input   = [Int]

-- | A test case collects a program and a list of answers together
--   with its expected result.
data TestCase = TestCase
  { testName    :: String
  , testInput   :: Input
  , testTrace   :: Trace Int
  , testResult  :: Result
  , testProgram :: Program
  }
              
instance Show TestCase
    where show tc = testName tc ++ " " ++ show (testInput tc) ++ 
                    " " ++ show (testResult tc)

tick :: State Int ()
tick = modify (1+)

-- | Running a program.
runProgram :: Program -> Input -> Trace Int -> State Int (Int, Int)
runProgram p inp t =
    do
      x <- play p t inp
      n <- get
      return (x, n)
    where
      play prog t inp =
          do
            r <- run prog t
            case r of
              Right x      -> return x
              Left (_, t') -> case inp of   --return what is in the trace so far
                                []       -> return $ sum (getTraceContent t')
                                a : inp' -> play prog (addAnswer a t') inp'

-- | Checking a test case. Compares expected and actual results.
checkTestCase :: TestCase -> Bool
checkTestCase (TestCase name i t r p) =
    r == evalState (runProgram p i t) 0

-- | List of interesting test cases.
testCases :: [TestCase]
testCases = [
     TestCase
    { testName    = "basic_test1"
    , testInput   = [0,0]
    , testTrace   = emptyTrace
    , testResult  = (0, 2)
    , testProgram = do
        liftR tick
        a <- ask () 
        b <- liftR (return 0)
        c <- ask () 
        liftR tick
        return (a + b + c)
    }
   , TestCase
    { testName    = "basic_test2"
    , testInput   = [4]
    , testTrace   = addResult "1" $
                    addAnswer 3 $
                    emptyTrace
    , testResult  = (8, 1)
    , testProgram = do
        a <- ask () -- replaced by 'Answer 3' in trace
        b <- liftR (return 1) -- replaced by 'Result "1"' in trace
        c <- ask () -- read from input
        liftR tick
        return (a + b + c)

    }
    , TestCase
    { testName    = "cut_test1"
    , testInput   = repeat 0 --works with an infinite list! ^_^
    , testTrace   = emptyTrace
    , testResult  = (0, 0)
    , testProgram = cut (return 0)
    }

    , TestCase
    { testName = "interrupted_test1"
    , testInput = [5]
    , testTrace = emptyTrace
    , testResult  = (5, 2)
    , testProgram = do   
        liftR tick
        reached <- ask () 
        liftR tick
        notReached <- ask () 
        return (reached + notReached) 
    }

    , TestCase --like basic_test2 but with extra ask added
    { testName    = "interrupted_test2"
    , testInput   = [4]
    , testTrace   = addResult "1" $
                    addAnswer 3 $
                    emptyTrace
    , testResult  = (8, 1)
    , testProgram = do
        a <- ask ()
        b <- liftR (return 1)
        c <- ask ()
        liftR tick
        ask ()
        return (a + b + c)
    }
  ]


--------------------------------------------------------------------------------
-- Generating arbitrary test cases

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
          nTicks = length $ filter (==Tick) l

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
          testTra = emptyTrace
      return $ TestCase testNam testInp testTra testRes testProg
          where
            toMonad (Tick)     = liftM (const 0) (liftR tick)
            toMonad (Return n) = liftR (return n)
            toMonad (Ask n)    = ask ()

instance Arbitrary TestCase where
    arbitrary = genTestCase
