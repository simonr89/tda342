module Replay (Replay
             , ReplayT
             , Trace
             , ask
             , io
             , run
             , liftR
             , emptyTrace
             , addResult
             , addAnswer) where

-- Types
type Replay q r a = ReplayT IO q r a 

newtype ReplayT m q r a = ReplayT {runReplayT :: Trace r -> m ((Either q a), Trace r) }

data Trace r = Trace { visited :: [Item r] -- ^ events up to now, in reverse order
                     , todo :: [Item r]    -- ^ events left to consume
                     }
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

{- liftR doesn't satisfy the monad transformer laws: 

  (1)   lift . return = return

   Our definition of return doesn't manipulate the trace, but liftR does.


  (2)   lift (m >>= f) = lift m >>= (lift . f)

   Here lift occurs once  on the left, and twice on the right.
   Using the same reasoning, on the LHS the trace is modified once, 
   and on the RHS twice.

   See the file Example.hs: 
    λ> running oneLift
    λ> running twoLifts

-}
liftR :: (Monad m, Show a, Read a) => m a -> ReplayT m q r a
liftR input = ReplayT $ \t -> do
               case todo t of
                 [] -> do a <- input
                          return (Right a, addResult t (show a))
                 (val:ts) -> case val of
                               Answer a -> fail "liftR"
                               Result str -> return (Right $ read str, visit t)

-- | Extract either an answer or a question with a trace, wrapped in
-- the underlying monad.
run      :: (Monad m) => ReplayT m q r a -> Trace r -> m (Either (q, Trace r) a)
run ra t = do (qora, t') <- (runReplayT ra) (resetTrace t)
              case qora of
                Left q -> return $ Left (q, t')
                Right a -> return $ Right a

-- | liftR specialised for IO actions
io :: (Show a, Read a) => IO a -> ReplayT IO q r a
io = liftR

-- | Try to answer the question with the trace.  If the trace is
-- empty, return the question and the used trace.
ask          :: (Monad m) => q -> ReplayT m q r r
ask question = ReplayT $ \t -> do
                 case todo t of
                   [] -> return (Left question, t)
                   (val:ts) -> case val of
                                 Answer a   -> return (Right a, visit t)
                                 Result str -> fail $ "ask: " ++ str

-- Helper functions for manipulating traces

emptyTrace :: Trace r
emptyTrace = Trace [] []

-- | Put the visited elements back at the beginning of the todo list
resetTrace :: Trace r -> Trace r
resetTrace (Trace v t) = Trace [] ((reverse v) ++ t)

-- | Add a result to the visited elements, assuming that the todo list is empty
addResult                  :: Trace r -> String -> Trace r
addResult (Trace v []) str =  Trace ((Result str):v) []

-- | Add an answer to the visited elements, assuming that the todo list is empty
addAnswer                :: Trace r -> r -> Trace r
addAnswer (Trace v []) r =  Trace ((Answer r):v) []

-- |Move the first todo element to the visited elements list
visit             :: Trace r -> Trace r
visit (Trace v t) = case t of
                      [] -> Trace v t
                      (x:xs) -> Trace (x:v) xs

