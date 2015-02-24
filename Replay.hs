module Replay (Replay
             , ReplayT
             , Trace(..)
             , ask
             , io
             , run
             , liftR
             , cut
             , emptyTrace
             , addResult
             , addAnswer
             , getAnswers) where

-- Types
type Replay q r a = ReplayT IO q r a 

newtype ReplayT m q r a = ReplayT {runReplayT :: Trace r -> m ((Either q a), Trace r) }

data Trace r = Trace { visited :: [Item r] -- ^ events up to now, in reverse order
                     , todo :: [Item r]    -- ^ events left to consume
                     }
    deriving (Show,Read)

data Item r = Answer r           -- ^ a user answer
            | Result String      -- ^ the result of a (m a) computation, stored with 'show'
            | Cut (Maybe String) -- ^ a cut, storing the result if it has been computed already
    deriving (Show,Read)

-- | Monad instance of ReplayT m q r.
instance (Monad m) => Monad (ReplayT m q r) where
    return x = ReplayT $ \t -> return (Right x, t)
    m >>=  k = ReplayT $ \t -> do (qora, t') <- runReplayT m t
                                  case qora of
                                     (Left q) -> return (Left q, t')
                                     (Right a) -> runReplayT (k a) t'

-- | Extract a result of a monadic computation and add it to the trace.
-- Require type a to be instances of Show and Read to be able to 
-- read from the trace and add new results to the trace. 

{- liftR doesn't satisfy the monad transformer laws: 

  (1)   liftR . return <> return

   Our definition of return doesn't manipulate the trace, but liftR does.


  (2)   liftR (m >>= f) <> liftR m >>= (liftR . f)

   Here lift occurs once  on the left, and twice on the right.
   Using the same reasoning, on the LHS the trace is modified once, 
   and on the RHS twice.

   See the file Example.hs: 
    λ> running oneLift
    λ> running twoLifts

-}
liftR :: (Monad m, Show a, Read a) => m a -> ReplayT m q r a
liftR input = ReplayT $ \t ->
              case todo t of
                [] -> do a <- input
                         return (Right a, addResult (show a) t)
                (Result str:_) -> return (Right $ read str, visit t)
                _ -> fail "mismatched trace: monad expected"

-- | Extract either an answer or a question with a trace, wrapped in
-- the underlying monad.
run      :: (Monad m) => ReplayT m q r a -> Trace r -> m (Either (q, Trace r) a)
run ra t = do (qora, t') <- runReplayT ra (resetTrace t)
              case qora of
                Left q -> return $ Left (q, t')
                Right a -> return $ Right a

-- | liftR specialised for IO actions
io :: (Show a, Read a) => IO a -> ReplayT IO q r a
io = liftR

-- | Try to answer the question with the trace.  If the trace is
-- empty, return the question and the used trace.
ask          :: (Monad m) => q -> ReplayT m q r r
ask question = ReplayT $ \t ->
               case todo t of
                 [] -> return (Left question, t)
                 (Answer a:_) -> return (Right a, visit t)
                 _ -> fail "mismatched trace: ask expected"

-- Trace r -> m ((Either q a), Trace r)
cut :: (Monad m, Read a, Show a) => ReplayT m q r a -> ReplayT m q r a
cut ra = ReplayT $ \t ->
         case todo t of
           [] -> runReplayT ra (addNewCut t)
           (Cut Nothing:_) -> do (qora, t') <- runReplayT ra (visit t)
                                 case qora of
                                   Left q -> return (Left q, t')
                                   Right a -> return (Right a, registerCut (show a) t')
           (Cut (Just str):_) -> return (Right $ read str, visit t)
           _ -> fail "mismatched trace: cut expected"

--Helper functions for manipulating traces

emptyTrace :: Trace r
emptyTrace = Trace [] []

-- | Put the visited elements back at the beginning of the todo list
resetTrace :: Trace r -> Trace r
resetTrace (Trace v t) = Trace [] ((reverse v) ++ t)

-- | Add a result to the visited elements, assuming that the todo list is empty
addResult                  :: String -> Trace r -> Trace r
addResult str (Trace v []) =  Trace ((Result str):v) []

-- | Add an answer to the visited elements, assuming that the todo list is empty
addAnswer                :: r ->Trace r -> Trace r
addAnswer r (Trace v []) =  Trace ((Answer r):v) []

addNewCut             :: Trace r -> Trace r
addNewCut (Trace v t) = Trace (Cut Nothing:v) t

registerCut                               :: String -> Trace r -> Trace r
registerCut str (Trace v t) = Trace (unstack v) t
    where unstack (Cut Nothing:vs) = Cut (Just str):vs
          unstack (_:vs)           = vs

-- | Move the first todo element to the visited elements list
visit             :: Trace r -> Trace r
visit (Trace v t) = case t of
                      [] -> Trace v t
                      (x:xs) -> Trace (x:v) xs

-- | Extract the answers of a Trace
getAnswers               :: Trace r -> [r]
getAnswers (Trace vis _) =  reverse $ foldr consAns [] vis
    where consAns (Answer r) rs = r:rs
          consAns (Result _) rs = rs
                      
