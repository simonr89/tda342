module Replay where

import Safe

-- Types
-- 
newtype Replay q r a = Replay { runReplay :: Trace r -> IO ((Either q a), Trace r) }

-- event up to now, events left to consume
data Trace r = Trace { visited :: [Item r], todo :: [Item r] }
             deriving (Show,Read)

data Item r = Answer r | Result String
              deriving (Show,Read)

-- Operations
instance Monad (Replay q r)
    where return = returnReplay
          (>>=) = bindReplay

returnReplay :: a -> Replay q r a
returnReplay x = Replay $ \t -> return (Right x, t)

-- (Trace r -> IO ((Either q a), Trace r) ->
-- (a -> (Trace r -> IO ((Either q b), Trace r))) ->
-- (Trace r -> IO ((Either q b), Trace r)
bindReplay     :: Replay q r a -> (a -> Replay q r b) -> Replay q r b
bindReplay m k = Replay $ \t -> do (qora, t') <- runReplay m t
                                   case qora of
                                     (Right a) -> runReplay (k a) t'
                                     (Left q) -> return (Left q, t')
                                             
io       :: (Show a, Read a) => IO a -> Replay q r a
io input = Replay $ \t -> do
             case todo t of
               [] -> do a <- input
                        return (Right a, addResult t (show a))
               (val:ts) -> case val of
                             Answer a -> fail "io"
                             Result str -> return (Right $ read str, addResult t str)
               
<<<<<<< HEAD
=======
       
ask          :: q -> Replay q r r
ask question = Replay $ \t -> do
                 case todo t of
                   [] -> return (Left question, t)
                   (val:ts) -> case val of
                                 Answer a -> return (Right a, addAnswer t a)
                                 Result str -> fail $ "ask" ++ str
>>>>>>> 06ff20f4c2d79d96596990366e9a4169bce5ec62
       
ask          :: q -> Replay q r r
ask question = Replay $ \t -> do
                 case todo t of
                   [] -> return (Left question, t)
                   (val:ts) -> case val of
                                 Answer a -> return (Right a, addAnswer t a)
                                 Result str -> fail $ "ask" ++ str

emptyTrace :: Trace r
emptyTrace = Trace [] []

<<<<<<< HEAD
resetTrace :: Trace r -> Trace r
resetTrace (Trace v t) = Trace [] (v ++ t)

=======
>>>>>>> 06ff20f4c2d79d96596990366e9a4169bce5ec62
addResult       :: Trace r -> String -> Trace r
addResult t str = Trace (visited t ++ [Result str]) (tailSafe $ todo t)
             
addAnswer     :: Trace r -> r -> Trace r
addAnswer t r = Trace (visited t ++ [Answer r]) (tailSafe $ todo t)

run      :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run ra t = do (qora, t') <- (runReplay ra) t
              case qora of
                Left q -> return $ Left (q, t')
                Right a -> return $ Right a
