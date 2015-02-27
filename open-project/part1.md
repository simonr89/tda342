# AFP -- Assignment 3

We are going to work on Grammatical Framework, a programming language for multilingual grammar applications. http://hackage.haskell.org/package/gf

GF is a language on its own, not just an EDSL for Haskell. Below some examples of usage:

```
$ gf LangEng.gf LangSwe.gf

Lang> parse -lang=Eng -cat=Cl "I am an apple" 
PredVP (UsePron i_Pron) (UseComp (CompCN (UseN apple_N)))
```

We load in the GF shell two files, `LangEng.gf` and `LangSwe.gf`, which are concrete syntaxes of an abstract syntax `Lang.gf`.
Then we can parse sentences in any of the two languages into abstract syntax trees.
The abstract syntax functions as an interlingua; by parsing in one language and linearising in another, we can
translate between languages.

```
Lang> parse -lang=Eng "I am an apple" | linearize -lang=Swe
jag är ett äpple
```

It can also generate random trees:

```
Lang> generate_random -tr | linearize
PhrUtt (PConjConj and_Conj) (UttCN (AdvCN (UseN hand_N) today_Adv)) please_Voc

and hand today please
och hand idag tack
```

The whole GF is a big package; counting all files that end in `.hs` in the `src/` directory results in 197 files.
The runtime consists of 30 modules with 5348 lines.
In our project, we are going to work on the random generation module.
We are aiming to optimize the random generation process by applying memoization.


## Learning outcomes

### DSL: design embedded domain specific languages
DSL.Concepts: (abstract) syntax, semantics, ...
Grammatical Framework, while being an independent language instead of an EDSL,

DSL.Implement: implement EDSLs in Haskell (as combinator libraries)

### Types: read, understand and extend Haskell programs which use advanced type system features
Types.Class: type classes, newtypes, deriving, ...
Types.GADT: (generalised) algebraic datatypes & type families
Types.HOT: functors, monads and monad transformers

### Spec: use specification based development techniques

This aspect is not directly related to our project or the GF package.
