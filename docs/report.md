AFP Lab 2 -- Report
Simon Robillard
Inari Listenmaa

The first section of this document describes the Replay monad.
Second part describes the web DSL and briefly explains our example application.

# The Replay Monad

Our Replay monad is a monad transformer of the following type:

```haskell
newtype ReplayT m q r a = 
     ReplayT {runReplayT :: Trace r -> m ((Either q a), Trace r) }
```

## Traces

The computation produces a trace that can be used as an argument 
for future replays. 
`q` is the type of question that can be asked of the user,
`a` is the return type of the underlying monad `m` and 
`r` is the type of elements recorded in traces.

The trace contains two lists of elements, wrapped in type `Item`.
When starting a replay with a non-empty string, all items are in
the `todo` list. Whenever an item is consumed, it is moved to the 
`visited` list.

```haskell
data Trace r = Trace { visited :: [Item r]
                     , todo :: [Item r]   
                     }
```

For handling traces, the module exports `emptyTrace`, the default
value for an empty trace, and the function `addAnswer` to add answers
to the trace.


```haskell
emptyTrace :: Trace r
addAnswer  :: Trace r -> r -> Trace r
```

## Replay API

The API provides the following functions:

```haskell
ask   :: q -> ReplayT m q r r
run   :: ReplayT m q r a -> Trace r -> m (Either (q, Trace r) a)
liftR :: (Monad m, Show a, Read a) => m a -> ReplayT m q r a
io    :: (Show a, Read a) => IO a -> ReplayT IO q r a
```

`ask` takes a value of type `q` and returns a Replay computation which
expects an answer from the user or from the trace.
`run` takes a Replay computation, a trace, and 

The intended usage is that whenever a computation is stopped,
the `run` function returns the latest question with the used trace.
The application using the replay monad will prompt the user with the
question, add the answer to the trace, and run the Replay monad again.


We implement the function `liftR` to lift a computation in the underlying monad
and add it to the trace. `io` is a specialised version, for IO as the
underlying monad.
However, Replay monad cannot be made an instance of
MonadTrans, because `liftR` doesn't satisfy the monad transformer laws: 


```haskell
lift . return == return
```

Our definition of return doesn't check the trace, but `liftR`
does. Given e.g. a malformed trace, the resulting monads will
have different behaviors.

```haskell
lift (m >>= f) == lift m >>= (lift . f)
```

Here lift occurs once  on the left, and twice on the right.
Using the same reasoning, on the LHS the trace is modified once, 
and on the RHS twice.

To demonstrate the behaviour, see the file `Example.hs`, 
with the functions `oneLift` and `twoLifts`.
`oneLift` contains the line

```haskell
liftR $ return 3 >>= (\n -> return (n+1))
```
and twoLifts contains the line  

```haskell
liftR (return 3) >>= liftR . (\n -> return (n+1))
```

```haskell 
> running oneLift
Trace {visited = [], todo = []}
Question: foo
Trace {visited = [Answer "foo",Result "4"], todo = []}
4

> running twoLifts 
Trace {visited = [], todo = []}
Question: foo
Trace {visited = [Answer "foo",Result "4",Result "3"], todo = []}
4
```

## Cut

In order to optimise the replay monad, we implement a cut operation.
The trace produced by that version will contain intermdiary results
only if the final result has not been computed. 
Otherwise all intermediary results are forgotten, leading to a more 
space-efficient trace and faster replays.

We add one more constructor to the type `Item` stored in trace:

```haskell
data Item r = Answer r
            | Result String
            | Cut (Maybe String)
```

Answer r is the answer of the type r, given by the user.
Result String is the result of a computation in the underlying monad,
stored as a string.
The third constructor in the `Item` data type is `Cut`, 
which stores the result if it has been computed already, otherwise
it contains `Nothing`.



# Web Programming


In the second part, we use the Replay monad to build web applications.
We send HTML forms to the user using 'ask', and the answers are stored 
in the trace of the Replay monad.

## A library for web forms

The type of our Web monad is Replay with IO as the underlying monad;
question type is a list of fields with a description, and
answer type is ____


```haskell
type Web a = Replay Question Answer a

data Question = Question { par :: Text 
                         , fields :: [Field]

type Answer = Map Text Text

data Field = Field { ident :: Text
                   , description :: Text
                   , visible :: Bool
                   }
```

We use the function `runWeb` to ...


## An interesting web program

Så spännande!

