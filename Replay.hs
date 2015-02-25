-- | A module for replayable computations
module Replay (Replay
             , ReplayT
             , Trace
             , ask
             , io
             , run
             , runDebug
             , liftR
             , cut
             , emptyTrace
             , addResult
             , addAnswer
             , addCut
             , getTraceContent)
             where

-- Types

-- | A monad transformer for replayable computations. The computation
-- produces a trace that can be used as an argument for future
-- replays. 'q' is the type of question that can be asked of the user,
-- 'a' is the return type of the underlying monad 'm' and 'r' is the
-- type of elements recorded in traces.
newtype ReplayT m q r a = 
                   ReplayT {runReplayT :: Trace r -> m ((Either q a), Trace r) }

-- | A version of replay T specialized for IO computation
type Replay q r a = ReplayT IO q r a

-- | The type of traces. Use the 'emptyTrace' and the 'addX' function
-- to build traces
data Trace r = Trace { visited :: [Item r] -- ^ events up to now, in reverse order
                     , todo :: [Item r]    -- ^ events left to consume
                     }
    deriving (Show,Read)

-- | The type of elements stored in traces
data Item r = Answer r       -- ^ a user answer
            | Result String  -- ^ the result of a (m a) computation, stored with 'show'
            | Cut String     -- ^ a cut, storing the result if it has been computed already
    deriving (Show,Read)

--------------------------------------------------------------------------------

-- | Monad instance of ReplayT m q r.
instance (Monad m) => Monad (ReplayT m q r) where
    return x = ReplayT $ \t -> return (Right x, t)
    m >>=  k = ReplayT $ \t -> do (qora, t') <- runReplayT m t
                                  case qora of
                                     (Left q)  -> return (Left q, t')
                                     (Right a) -> runReplayT (k a) t'


-- | Extract a result of a monadic computation and add it to the
-- trace.  Require type a to be instances of Show and Read to be able
-- to read from the trace and add new results to the trace.
liftR :: (Monad m, Show a, Read a) => m a -> ReplayT m q r a
liftR input = ReplayT $ \t ->
              case todo t of
                [] -> do a <- input
                         return (Right a, addResult (show a) t)
                (Result str:_) -> return (Right $ read str, visit t)
                _ -> fail "mismatched trace: Result expected"

-- | Extract either an answer or a question with a trace, wrapped in
-- the underlying monad.
run      :: (Monad m) => ReplayT m q r a -> Trace r -> m (Either (q, Trace r) a)
run ra t = do (qora, t') <- runReplayT ra (resetTrace t)
              case qora of
                Left q -> return $ Left (q, t')
                Right a -> return $ Right a


-- | Run function for debugging purposes:
--   returns the trace also with succesful computation
runDebug      :: (Monad m) => ReplayT m q r a -> Trace r -> m (Either q a, Trace r)
runDebug ra t = do (qora, t') <- runReplayT ra (resetTrace t)
                   case qora of
                      Left q -> return $ (Left q, t')
                      Right a -> return $ (Right a, t')

-- | liftR specialised for IO actions
io :: (Show a, Read a) => IO a -> ReplayT IO q r a
io = liftR

-- | Try to answer the question with the trace.  If the trace is
-- empty, return the question and the used trace.
ask          :: (Monad m, Read r) => q -> ReplayT m q r r
ask question = ReplayT $ \t ->
               case todo t of
                 [] -> return (Left question, t)
                 (Answer a:_) -> return (Right a, visit t)
                 _ -> fail "mismatched trace: Answer expected"

-- | Generate an optimized version of a replay monad. The trace
-- produced by that version will contain intermediary results only if
-- the final result has not been computed. Otherwise all intermediary
-- results are forgotten, leading to a more space-efficient trace and
-- faster replays
cut :: (Monad m, Read a, Show a) => ReplayT m q r a -> ReplayT m q r a
cut ra = ReplayT $ \t ->
         case todo t of
           (Cut s:_) -> return (Right $ read s, visit t)
           _         -> do (qora, t') <- runReplayT ra t
                           case qora of
                             Right a -> return (Right a, addCut (show a) emptyTrace)
                             Left q  -> return (Left q, t')


--------------------------------------------------------------------------------
--Helper functions for manipulating traces

-- | The initial, empty trace
emptyTrace :: Trace r
emptyTrace = Trace [] []

-- | Put the visited elements back at the beginning of the todo list
resetTrace :: Trace r -> Trace r
resetTrace (Trace v t) = Trace [] ((reverse v) ++ t)

-- | Add a result to the visited elements, assuming that the todo list
-- is empty
addResult                  :: String -> Trace r -> Trace r
addResult str (Trace v []) =  Trace (Result str:v) []

-- | Add an answer to the visited elements, assuming that the todo
-- list is empty
addAnswer                :: r ->Trace r -> Trace r
addAnswer r (Trace v []) =  Trace (Answer r:v) []

-- | Add a cut to the visited elements, assuming that the todo
-- list is empty
addCut                :: String -> Trace r -> Trace r
addCut str (Trace v []) =  Trace (Cut str:v) []

-- | Move the first todo element to the visited elements list
visit             :: Trace r -> Trace r
visit (Trace v t) = case t of
                      [] -> Trace v t
                      (x:xs) -> Trace (x:v) xs

-- | Extract the contents of a Trace.
--   Returns the values of all Answers and non-() Results and Cuts.
getTraceContent               :: (Read r) => Trace r -> [r]
getTraceContent (Trace vis _) =  reverse $ foldr consAns [] vis
    where consAns (Answer r) rs = r:rs
          consAns (Result r) rs = case r of
                                   "()" -> rs
                                   _    -> (read r):rs
          consAns (Cut str)  rs =  consAns (Result str) rs 
