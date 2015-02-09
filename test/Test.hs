{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.QuickCheck hiding (Result)
import Control.Monad.State.Lazy
import Data.Monoid
import Data.IORef
import Replay
import System.Exit

type ReplayTest a = ReplayT (State Int) () Int a 

instance (Show a) => Show (ReplayTest a) where
    show x = "replay"


-- | Runs the test suite for the replay library
main :: IO ()
main = do
  quickCheck unit_right
  -- results <- runTests
  -- if and results
  --   then return ()
  --   else exitFailure

-- | Programs are parameterised over a 'tick' action.
--   Questions are () and answers are integers.
type Program = IO () -> ReplayTest Int

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

-- | Running a program.
-- runProgram :: Program -> Input -> IO Result
-- runProgram p inp = do
--     counter <- newIORef 0
--     let tick = modifyIORef counter (+1)
--     x <- play (p tick) emptyTrace inp
--     n <- readIORef counter
--     return (x, n)
--   where
--     play prog t inp = do
--       r <- run prog t
--       case r of
--         Right x      -> return x
--         Left (_, t') -> case inp of
--           []       -> error "too few inputs"
--           a : inp' -> play prog (addAnswer t' a) inp'

-- | Checking a test case. Compares expected and actual results.
-- checkTestCase :: TestCase -> IO Bool
-- checkTestCase (TestCase name i r p) = do
--   putStr $ name ++ ": "
--   r' <- runProgram p i
--   if r == r'
--     then putStrLn "ok" >> return True
--     else putStrLn ("FAIL: expected " ++ show r ++
--                   " instead of " ++ show r')
--          >> return False


-- | List of interesting test cases.
testCases :: [TestCase]
testCases = undefined
  -- [ TestCase
  --   { testName    = "test1"
  --   , testInput   = [3,4]
  --   , testResult  = (8, 1)
  --   , testProgram = \tick -> do
  --       io tick
  --       a <- ask () -- should be 3
  --       b <- io (return 1)
  --       c <- ask () -- should be 4
  --       return (a + b + c)
  --   }
  --  , TestCase
  --   { testName  = "test2"
  --   , testInput   = [0,0]
  --   , testResult  = (0, 2)
  --   , testProgram = \tick -> do
  --       io tick
  --       a <- ask () -- should be 0
  --       b <- io (return 0)
  --       c <- ask () -- should be 0
  --       io tick
  --       return (a + b + c)
  --   }
  -- ]

unit_right :: ReplayTest Int -> Bool
unit_right m = undefined --(m >>= return) == m


instance (Arbitrary a) => Arbitrary (ReplayTest a) where
   arbitrary = do x <- arbitrary
                  elements [return x]

-- | Running all the test cases.
--runTests = mapM checkTestCase testCases
