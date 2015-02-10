module Replay where

import Safe

-- Types
-- 
type Replay q r a = ReplayT IO q r a 

newtype ReplayT m q r a = ReplayT {runReplayT :: Trace r -> m ((Either q a), Trace r) }

liftR :: (Monad m, Show a, Read a) => m a -> ReplayT m q r a
liftR m = ReplayT $ \t -> do a <- m
                             return (Right a, t)

instance (Monad m) => Monad (ReplayT m q r) where
    return x = ReplayT $ \t -> return (Right x, t)
    m >>=  k = ReplayT $ \t -> do (qora, t') <- runReplayT m t
                                  case qora of
                                     (Right a) -> runReplayT (k a) t'
                                     (Left q) -> return (Left q, t')

-- event up to now, events left to consume
data Trace r = Trace { visited :: [Item r], todo :: [Item r] }
             deriving (Show,Read)

data Item r = Answer r | Result String
              deriving (Show,Read)

                                             
io       :: (Show a, Read a, Monad m) => m a -> ReplayT m q r a
io input = ReplayT $ \t -> do
             case todo t of
               [] -> do a <- input
                        return (Right a, addResult t (show a))
               (val:ts) -> case val of
                             Answer a -> fail "io"
                             Result str -> return (Right $ read str, addResult t str)
                      
ask          :: (Monad m) => q -> ReplayT m q r r
ask question = ReplayT $ \t -> do
                 case todo t of
                   [] -> return (Left question, t)
                   (val:ts) -> case val of
                                 Answer a -> return (Right a, addAnswer t a)
                                 Result str -> fail $ "ask" ++ str

emptyTrace :: Trace r
emptyTrace = Trace [] []

resetTrace :: Trace r -> Trace r
resetTrace (Trace v t) = Trace [] (v ++ t)

addResult       :: Trace r -> String -> Trace r
addResult t str = Trace (visited t ++ [Result str]) (tailSafe $ todo t)
             
addAnswer     :: Trace r -> r -> Trace r
addAnswer t r = Trace (visited t ++ [Answer r]) (tailSafe $ todo t)

run      :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run ra t = do (qora, t') <- (runReplayT ra) (resetTrace t)
              case qora of
                Left q -> return $ Left (q, t')
                Right a -> return $ Right a
