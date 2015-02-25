module Example where

import Replay
import Data.Time

trace =
    addResult "2015-02-05 15:05:08.780997 UTC" $
    addResult "()" $
    addAnswer "Simon" $
    addResult "()" $
    addAnswer "27" $
    addResult "()" $
    addResult "2015-02-05 15:05:08.780997 UTC" $
    emptyTrace


example :: Replay String String Int
example = do
  t0 <- io getCurrentTime
  io (putStrLn "Hello!")
  age <- ask "What is your age?"
  io (putStrLn ("You are " ++ age))
  name <- ask "What is your name?"
  io (putStrLn (name ++ " is " ++ age ++ " years old"))
  t1 <- io getCurrentTime
  io (putStrLn ("Total time: " ++ show (diffUTCTime t1 t0)))
  return (read age)

oneLift :: Replay String String Int
oneLift = do
  foo <- liftR $ return 3 >>= (\n -> return (n+1))
  ask "Say something. Next line should show just one 4 in trace."
  return foo

twoLifts :: Replay String String Int
twoLifts = do
  foo <- liftR (return 3) >>= liftR . (\n -> return (n+1))
  ask "Say something. Next line should show 3 and 4 in trace."
  return foo

running :: Replay String String a -> IO a
running prog = play emptyTrace
 where
  play t = do
    print t
    r <- run prog t    -- this is the same prog every time!
    case r of
      Left (q, t') -> do
        putStr ("Question: " ++ q ++ " ")
        r <- getLine
        play $ addAnswer r t'
      Right x      -> return x

askAge :: ReplayT IO String String Integer
askAge = do
     birth <- ask "What is your birth year?"
     now <- io getCurrentTime
     let (year, _month, _day) = toGregorian $ utctDay now
     return (year - read birth)
