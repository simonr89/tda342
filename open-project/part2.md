% \pagestyle{fancy}
% \lhead{\textcolor{gray}{Simon Robillard \& Inari Listenmaa}}
% \rhead{\textcolor{gray}{Advanced Functional Programming}}
% \lfoot{\textcolor{gray}{}}
% \rfoot{\thepage}
% \renewcommand{\headrulewidth}{0.5pt} 
% \renewcommand{\footrulewidth}{0.5pt} 
% \fancyfoot[C]{\footnotesize \textcolor{gray}{}} 

% \centerline{ {\Large \bf Assignment 3 -- part II} }
% \vspace*{0.2cm}

As described in part I, we have worked on the Grammatical Framework package. However, instead of modifying the original random generation module, we have created our own module, based on [Duregård et al., 2012](http://dl.acm.org/citation.cfm?id=2364515). 
We have worked on a module called [PGF](http://hackage.haskell.org/package/gf-3.6/docs/PGF.html): an API meant to be used for embedding GF grammars in Haskell programs.
It contains functions to load and interpret grammars compiled in Portable Grammar Format (PGF). The PGF format is produced as a final output from the GF compiler. 

Our contributions are thus not visible to a user who is writing GF code and using the GF shell, but rather to a user who is loading a grammar and writing a Haskell program to generate trees.


# Library interface

The grammars are represented by a data type PGF, which includes the following information:

```haskell
data PGF = PGF {
  absname   :: CId ,
  abstract  :: Abstr ,
  concretes :: Map.Map CId Concr
  }
```

The types `Abstr` and `Concr` are not visible in the API; however, there are a number of functions that take a PGF as an argument and give relevant information. Some examples include

```haskell
-- | List of all languages available in the given grammar
languages :: PGF -> [Language]

-- | List of all categories defined in the given grammar.
categories :: PGF -> [CId]

-- | List of all functions defined in the abstract syntax
functions :: PGF -> [CId]
```

The following are relevant for our purposes of enumerating expressions:

```haskell
-- | List of all functions defined for a given category
functionsByCat :: PGF -> CId -> [CId]


-- | The type of a given function
functionType :: PGF -> CId -> Maybe Type
```

Parsing and linearization have variants where more information is included, such as probability of the parse and trying with all languages.

```haskell
-- | Tries to parse the given string in the specified language and to produce abstract syntax expression.
parse :: PGF -> Language -> Type -> String -> [Tree]


-- | Linearizes given expression as string in the language
linearize :: PGF -> Language -> Tree -> String
```

The function `generateAll` generates an exhaustive, possibly infinite, list of abstract syntax expressions.
Other functions include variants such as taking a template (e.g. "Noun verb adjective noun"), or limit generation to a specified depth. 

```haskell
generateAll :: PGF -> Type -> [Expr]
generateAllDepth :: PGF -> Type -> Maybe Int -> [Expr]
generateFrom :: PGF -> Expr -> [Expr]
generateFromDepth :: PGF -> Expr -> Maybe Int -> [Expr]
generateRandom :: RandomGen g => g -> PGF -> Type -> [Expr]
```

Our contributions follow the style of the API: we've 

```haskell
 enumerateAll,         enumerateAllDepth
         , enumerateRandom,      enumerateRandomDepth
```






# Library implementation



## Expressions

## Random generation



# Analysis of our code

The Haskell library `testing-feat` enables the user to construct an
enumeration of an algebraic datatype T as a function `Nat -> T`. The
values produced by the enumeration are useful for testing, either
systematic (e.g. with SmallCheck) or random (e.g. with
QuickCheck).

Our contribution to GF is the module `Enumerate`, which implements
(some of) the same functionalities as the module `Generate`, but using
a principle based on FEAT. The whole GF source code, modified to
include functions in Enumerate, is available on
github.com/inariksit/GF](https://github.com/inariksit/GF) and can be
installed using the `Makefile` provided with the source code.

Our code is based on a minimal implementation of FEAT ideas written by
Koen Claessen's and available at
[github.com/koengit/feat/Feat.hs](https://github.com/koengit/feat/blob/master/Feat.hs). In
contrast to the actual implementation of FEAT, this one uses a deep
embedding to represent functional enumerations of datatypes, as well
as explicit memoization.

FEAT requires the user to represent the constructors of a datatype as
members of the `Constructor` in order to produce a functional
enumeration of that type. Alternatively, it is possible to use
Template Haskell to perform compile-time introspection over that
datatype. In our case however, the datatype (the grammar) is already
described by a structure of the type `PGF`, which is to produce the
enumeration. In fact having two separate deep embeddings, one for the
grammar itself, and one for its enumeration, is a bit superfluous. As
future work, it could be interesting to integrate the enumeration more
tightly into the grammar representation.

Compared to `Generate`, the module `Enumerate` improves the
performance of term enumeration. However it is limited to simply typed
terms, whereas `Generate` can produce dependently typed terms. It is
still unclear whether the same technique could be used to geenrate
dependently typed terms, but it is an interesting lead for future
work.
