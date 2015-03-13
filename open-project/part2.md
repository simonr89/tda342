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

# Introduction to Grammatical Framework

Grammatical Framework (GF) is a domain-specific programming language designed to write multilingual grammars. Each program consists of an abstract syntax, and 0 or more concrete syntaxes. Below is an example of a "Hello world" grammar.

```haskell
abstract Hello = {

flags startcat=Greeting ;

cat
  Greeting ;
  Recipient ;
fun
  Hello   : Recipient -> Greeting ;
  World   : Recipient ;
  Friends : Recipient ;
  Dear    : Recipient -> Recipient ;
}
```
The grammar has categories `Greeting` and `Recipient`, and functions that build values of these categories. The start category is `Greeting`.

In a concrete syntax, we give the categories concrete definitions as records that consist of strings, tables and parameters; everything that is needed to represent morphological and syntactical properties of the categories. In the English example below, we are doing fine with just string concatenation:

```haskell
concrete HelloEng of Hello = {

lincat
  Greeting, Recipient = {s : Str} ;
lin
  Hello rec = {s = "hello" ++ rec.s} ;
  World     = {s = "world"} ;
  Friends   = {s = "friends"} ;
  Dear  rec = {s = "dear" ++ rec.s} ;
}
```

In Icelandic, greeting agrees with the number of the recipient, so we need to encode this in our types: `Greeting` remains as a record with string, but `Recipient` includes a field to indicate number. The function `Hello` chooses appropriate form depending on its argument.

```haskell
concrete HelloIce of Hello = {
...
}
```

We use this grammar as an example in documenting the library interface.

# Library interface

The grammars are represented by a data type PGF, which includes the following information:

```haskell
data PGF = PGF {
  absname   :: CId ,
  abstract  :: Abstr ,
  concretes :: Map.Map CId Concr
  }
```

The types `Abstr` and `Concr` are not visible in the API; however, there are a number of functions that take a PGF as an argument and return relevant information. For example, one can get a list of categories, functions and languages of a given grammar.

```haskell
-- | List of all languages available in the given grammar
languages :: PGF -> [Language]

-- | List of all categories defined in the given grammar.
categories :: PGF -> [CId]

-- | List of all functions defined in the abstract syntax
functions :: PGF -> [CId]

```

Both categories and functions are represented by a type `CId`, which is just a newtype wrapper for a ByteString. In addition to `functions`, the library provides `functionType` which takes a PGF and an identifier, and gives the type of that identifier.


```haskell
-- | The type of a given function
functionType :: PGF -> CId -> Maybe Type

-- | List of all functions defined for a given category
functionsByCat :: PGF -> CId -> [CId]

```

Applying `functions` to our Hello grammar, we get the result `[Dear,Friends,Hello,World]`. Applying `functionType` to `World` we get `DTyp [] Recipient`, and to `Hello` we get `DTyp [Recipient] Greeting`, meaning that the type that we get from applying `Hello` is a greeting, and it needs an argument of type `Recipient`.

`functionsByCat` takes a category as an argument, and returns a list of all functions whose return type is that category. For `Recipient`, it returns `[Dear,Friends,World]`. `Dear` takes an argument and the rest two don't, but since all of them have `Recipient` as the return type, they are returned in the same list.


The core functions of a grammar are parsing and linearization. The API contains also variants where more information is included, such as probability of the parse and trying with all languages.

```haskell
-- | Tries to parse the given string in the specified language and to produce abstract syntax expression.
parse :: PGF -> Language -> Type -> String -> [Tree]


-- | Linearizes given expression as string in the language
linearize :: PGF -> Language -> Tree -> String
```

The generation module provides both exhaustive and random generation of trees. 
There are variants such as taking a template (e.g. only generate trees of form `noun verb adjective noun`), or limit generation to a specified depth. 

Our contributions follow the style of the API: we export the following functions, which all have a counterpart in the library with prefix `generate` instead of `enumerate`.

```haskell
-- | Enumerates an exhaustive possibly infinite list of
-- abstract syntax expressions.
enumerateAll :: PGF -> Type -> [Expr]

-- | A variant of 'enumerateAll' which also takes as argument
-- the upper limit of the depth of the enumerated expression.
enumerateAllDepth :: PGF -> Type -> Maybe Int -> [Expr]

-- | Enumerates an infinite list of random abstract syntax expressions.
-- The size of a space is also generated randomly.
enumerateRandom :: RandomGen g => g -> PGF -> Type -> [Expr]

-- | A variant of 'enumerateRandom' which also takes as argument
-- the upper limit of the depth of the enumerated expression.
enumerateRandomDepth :: RandomGen g => g -> PGF -> Type -> Int -> [Expr]

-- | Enumerates a list of random abstract syntax expressions.
-- The size of a space is given as an argument.
enumerateRandomSized :: RandomGen g => g -> Integer -> PGF -> Type -> [Expr]
```

The function `enumerateRandomSized` that takes a size as an argument isn't in the original, but we added it, because it depends on grammar which sizes are interesting. We discuss this more in section TODO.

In addition to the aspects described above, the library includes functions for a wide range of tasks, including morphological analysis, parsing with probabilities and graphical visualization. The full documentation of the library is in [http://hackage.haskell.org/package/gf-3.6/docs/PGF.html](hackage.haskell.org/package/gf-3.6/docs/PGF.html).

~~~~~ 
Move this bit to the section TODO:

Size `n` includes all functions with n constructors: for the hello grammar, size 1 includes the trees `Hello` and `World`, size 2 has `Hello World`, `Hello Friends`, `Dear World` and `Dear Friends`. All bigger sizes have 4 trees; the exact same ones as size 2, but an extra `Dear` inserted for each size.

As a demonstration, we show the number of trees with different amount of constructors for the first 20 sizes. For some grammars, random generating a size would be quite likely to end up in a size that has 0 trees.

```haskell
λ> let helloSizes = map (\i -> size i (mkSpace' "Hello.pgf")) [1..]
λ> let duckSizes = map (\i -> size i (mkSpace' "Duck.pgf")) [1..]
λ> let teleSizes = map (\i -> size i (mkSpace' "Telescope.pgf")) [1..]
λ> map card (take 20 helloSizes)
[2,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4]
λ> map card (take 20 duckSizes)
[6,3,2,7,11,18,30,46,63,87,110,141,171,208,246,288,335,381,438,487]
λ> map card (take 20 teleSizes)
[5,3,0,6,0,0,18,0,27,0,0,135,0,189,0,0,1134,0,1539,0]
```

Depth is like size, but everything that is included in depth `n-1` is also included in depth `n`. 

~~~~~~~


# Library implementation

## Expressions

We generate an enumeration of GF abstract syntax trees, represented by a type `Expr`. Below is a definition of the type:

```haskell
data Expr =
   EAbs BindType CId Expr           -- ^ lambda abstraction
 | EApp Expr Expr                   -- ^ application
 | ELit Literal                     -- ^ literal
 | EMeta  MetaId                    -- ^ meta variable
 | EFun   CId                       -- ^ function or data constructor
 | EVar   Int                       -- ^ variable with de Bruijn index
 | ETyped Expr Type                 -- ^ local type signature
 | EImplArg Expr                    -- ^ implicit argument in expression
```

Most of the definition wasn't relevant for our task. For instance, meta variables are used in an abstract syntax with dependent types, which is out of scope for our project. We don't need literals either, since we want to construct enumerations of the functions defined in the grammar.
The constructors we did use are `EApp` and `EFun`; this is enough to represent all trees in our enumerations.

To demonstrate the difference, here is a sentence parsed in the GF shell:

```haskell
Duck> p "she made her duck"
PredVP she_Pron (V2VtoVP make_causative_V2V she_Pron duck_V)
PredVP she_Pron (V2toVP make_V2 (PossNP she_Pron duck_N))
PredVP she_Pron (V3toVP make_benefactive_V3 she_Pron (MassNP duck_N))
```

and here in ghci, using PGF library:
```haskell
λ> mapM_ print $ parse duck eng s "she made her duck"
EApp (EApp (EFun PredVP) (EFun she_Pron)) (EApp (EApp (EApp (EFun V2VtoVP) (EFun make_causative_V2V)) (EFun she_Pron)) (EFun duck_V))
EApp (EApp (EFun PredVP) (EFun she_Pron)) (EApp (EApp (EFun V2toVP) (EFun make_V2)) (EApp (EApp (EFun PossNP) (EFun she_Pron)) (EFun duck_N)))
EApp (EApp (EFun PredVP) (EFun she_Pron)) (EApp (EApp (EApp (EFun V3toVP) (EFun make_benefactive_V3)) (EFun she_Pron)) (EApp (EFun MassNP) (EFun duck_N)))
```

In the original grammar, we have zero-place functions (e.g. `she_Pron :: Pron`, `duck_V :: V`), one-place functions (e.g. `MassNP :: N -> NP`), up to three-place functions (e.g. `V3toVP :: V3 -> NP -> NP -> VP`). The functions in the PGF library are curried: all are `CId`s that get applied with `EApp`, no matter what is the arity of the function in the original GF grammar.


## Random generation



# Analysis of our code

Our code is in the file `Enumerate.hs`. The whole GF source code, modified to include functions in Enumerate, is in [github.com/inariksit/GF](https://github.com/inariksit/GF) and can be installed by `cabal install`.

A large portion of the code in `Enumerate` is taken from Koen Claessen's implementation here: [github.com/koengit/feat/Feat.hs](https://github.com/koengit/feat/blob/master/Feat.hs). The whole data type `Space a` and `Cache a` is taken from there, along with the functions `app`, `datatype`, `choice`, `card`, `size`, `index` and `depth`.
We have implemented ourselves the functions `mkSpace` and `liftS2`.

