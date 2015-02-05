module Replay where

-- Types
data Replay q r a = Replay { trace :: Trace r
                           , val :: a}

type Trace r = [Item r]

data Item r = Answer r | Result String
              deriving (Show,Read)

-- Operations
instance Monad (Replay q r)
    where return = returnReplay
          (>>=) = bindReplay

returnReplay :: a -> Replay q r a
returnReplay = error "undefined"

bindReplay :: Replay q r a -> (a -> Replay q r b) -> Replay q r b
bindReplay = error "undefined"

io :: (Show a, Read a) => IO a -> Replay q r a
io = error "undefined"
       
ask :: q -> Replay q r r
ask  = error "undefined"
       
emptyTrace :: Trace r
emptyTrace = []
             
addAnswer  :: Trace r -> r -> Trace r
addAnswer t r = t ++ [Answer r]

run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run  = error "undefined"
