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

-- | List of all categories defined in the given grammar. The categories are defined in the abstract syntax with the 'cat' keyword.
categories :: PGF -> [CId]

-- | List of all functions defined in the abstract syntax
functions :: PGF -> [CId]
```

Especially relevant are the following:

```haskell
-- | List of all functions defined for a given category
functionsByCat :: PGF -> CId -> [CId]Source


-- | The type of a given function
functionType :: PGF -> CId -> Maybe TypeSource
```






# Library implementation



## Expressions

## Random generation



# Analysis of our code

Our code is in the file `Enumerate.hs`. The whole GF source code, modified to include functions in Enumerate, is in github.com/inariksit/GF](https://github.com/inariksit/GF) and can be installed by `cabal install`.

A large portion of the code in `Enumerate` is taken from Koen Claessen's implementation here: [github.com/koengit/feat/Feat.hs](https://github.com/koengit/feat/blob/master/Feat.hs). The whole data type `Space a` and `Cache a` is taken from there, along with the functions `app`, `datatype`, `choice`, `card`, `size`, `index` and `depth`.
We have implemented ourselves the functions `mkSpace` and `liftS2`.

