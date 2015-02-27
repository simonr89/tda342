module Example where

import Replay


-- | Simple example program
askAge :: ReplayT IO String String Integer
askAge = do
    birth <- ask "What is your birth year?"
    now <- ask "What year is it now?"
    return (read now - read birth)


running :: Replay String String a -> IO a
running prog = play emptyTrace
 where
  play t = do
    r <- run prog t    -- this is the same prog every time!
    case r of
      Left (q, t') -> do
        putStr ("Question: " ++ q ++ " ")
        r <- getLine
        play $ addAnswer r t'
      Right x      -> return x


runningDebug :: Replay String String a -> IO (a, Trace String)
runningDebug prog = play emptyTrace
 where
  play t = do
    r <- runDebug prog t
    case r of
      (Left q, t') -> do
        print t'
        putStr ("Question: " ++ q ++ " ")
        r <- getLine
        play $ addAnswer r t'
      (Right x, t') -> return (x, t')

testCut :: (Show a, Read a) => Replay String String a -> IO ()
testCut prog = do 
    let cutProg = cut prog
    (res,cutTrace) <- runningDebug $ cutProg
    putStrLn $ "Results of the first cut: " ++ show (res,cutTrace)
    putStrLn $ "Running again with the normal run function:"
    newRes <- run cutProg cutTrace
    print newRes


