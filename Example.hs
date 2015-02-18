module Example where

import Replay
import Data.Time

trace = Trace [] [Result "2015-02-05 15:05:08.780997 UTC",
                  Result "()", Answer "27", Result "()", Answer "Simon", Result "()",
                  Result "2015-02-05 15:05:08.780997 UTC"]


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
        play $ substAnswer t' r
      Right x      -> return x
