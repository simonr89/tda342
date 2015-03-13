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

Our code is in the file `Enumerate.hs`. The whole GF source code, modified to include functions in Enumerate, is in github.com/inariksit/GF](https://github.com/inariksit/GF) and can be installed by `cabal install`.

A large portion of the code in `Enumerate` is taken from Koen Claessen's implementation here: [github.com/koengit/feat/Feat.hs](https://github.com/koengit/feat/blob/master/Feat.hs). The whole data type `Space a` and `Cache a` is taken from there, along with the functions `app`, `datatype`, `choice`, `card`, `size`, `index` and `depth`.
We have implemented ourselves the functions `mkSpace` and `liftS2`.

