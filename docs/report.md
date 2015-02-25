# AFP Lab 2 -- Report

The first section of this document describes the Replay monad.
Second part describes the web DSL and briefly explains our example application.

## Part I - The Replay Monad

Our Replay monad is a monad transformer of the following type:

```haskell

newtype ReplayT m q r a = 
   ReplayT {runReplayT :: Trace r -> m ((Either q a), Trace r) }
```

The computation produces a trace that can be used as an argument 
for future replays. 
`q` is the type of question that can be asked of the user,
`a` is the return type of the underlying monad `m` and 
`r` is the type of elements recorded in traces.

The trace contains ...

```haskell

data Trace r = Trace { visited :: [Item r]
                     , todo :: [Item r]   
                     }
```

We implement the function `liftR` to lift a computation in the underlying monad
and add it to the trace. However, Replay monad cannot be made an instance of
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

See the file Example.hs, with the functions oneLift and twoLifts.
oneLift contains the line

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

## Part II - Task 1 - A library for web forms


In the second part, we use the Replay monad to build web applications.
We send HTML forms to the user using 'ask', and the answers are stored 
in the trace of the Replay monad.

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


## Part II - Task 2 - Optimising the Replay monad


We implement a cut operation in order to optimise the replay monad.
The trace produced by that version will contain intermdiary results
only if the final result has not been computed. 
Otherwise all intermediary results are forgotten, leading to a more 
space-efficient trace and faster replays.

We add one more constructor to the trace: ...


