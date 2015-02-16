module Replay where -- (Replay(..),ReplayT,Trace(..),ask,io,run, liftR, emptyTrace, addAnswer) where

import Safe

-- Types
type Replay q r a = ReplayT IO q r a 

newtype ReplayT m q r a = ReplayT {runReplayT :: Trace r -> m ((Either q a), Trace r) }

                     -- event up to now,    events left to consume
data Trace r = Trace { visited :: [Item r], todo :: [Item r] }
    deriving (Show,Read)

data Item r = Answer r | Result String
    deriving (Show,Read)

-- | Monad instance of ReplayT m q r.
instance (Monad m) => Monad (ReplayT m q r) where
    return x = ReplayT $ \t -> return (Right x, t)
    m >>=  k = ReplayT $ \t -> do (qora, t') <- runReplayT m t
                                  case qora of
                                     (Right a) -> runReplayT (k a) t'
                                     (Left q) -> return (Left q, t')


-- | Extract a result of a monadic computation and add it to the trace.
-- Require type a to be instances of Show and Read to be able to 
-- read from the trace and add new results to the trace. 
-- liftR doesn't satisfy the monad transformer laws: 
--    liftR . return = return
--    liftR (m >>= f) = liftR m >>= (liftR . f)
-- In the first law, we can see from the definition of return that it doesn't manipulate the trace, but liftR does.
-- As for the second law, lift occurs once  on the left hand side, and twice on the right hand side.
-- Using the same reasoning, on the LHS the trace is modified once, and on the RHS twice.


liftR :: (Monad m, Show a, Read a) => m a -> ReplayT m q r a
liftR input = ReplayT $ \t -> do
               case todo t of
                 [] -> do a <- input
                          return (Right a, addResult t (show a))
                 (val:ts) -> case val of
                               Answer a -> fail "liftR"
                               Result str -> return (Right $ read str, addResult t str)

-- | Exctract either an answer or a question with a trace,
-- wrapped in the underlying monad.
run      :: (Monad m) => ReplayT m q r a -> Trace r -> m (Either (q, Trace r) a)
run ra t = do (qora, t') <- (runReplayT ra) (resetTrace t)
              case qora of
                Left q -> return $ Left (q, t')
                Right a -> return $ Right a

-- | liftR specialised for IO actions
io :: (Show a, Read a) => IO a -> ReplayT IO q r a
io = liftR

-- | Try to answer the question with the trace. 
-- If the trace is empty, return the question and the used trace.
ask          :: (Monad m) => q -> ReplayT m q r r
ask question = ReplayT $ \t -> do
                 case todo t of
                   [] -> return (Left question, t)
                   (val:ts) -> case val of
                                 Answer a   -> return (Right a, addAnswer t a)
                                 Result str -> fail $ "ask: " ++ str


-- Helper functions for manipulating traces

emptyTrace :: Trace r
emptyTrace = Trace [] []

resetTrace :: Trace r -> Trace r
resetTrace (Trace v t) = Trace [] (v ++ t)

addResult       :: Trace r -> String -> Trace r
addResult t str = Trace (visited t ++ [Result str]) (tailSafe $ todo t)
             
addAnswer     :: Trace r -> r -> Trace r
addAnswer t r = Trace (visited t ++ [Answer r]) (tailSafe $ todo t)

