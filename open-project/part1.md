# AFP -- Assignment 3

We are going to work on Grammatical Framework (GF), a programming language for multilingual grammar applications. http://hackage.haskell.org/package/gf

GF is a DSL, but it's an independent language instead of an EDSL for Haskell. Below some examples of usage:

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

The whole GF is a large package; counting all `.hs` files in the `src` directory results in 197 files.
The runtime consists of 30 modules with 5348 lines.
In our project, we are going to work on the random generation module.
We are aiming to optimize the random generation process by applying memoization.


## Learning outcomes

### DSL: design embedded domain specific languages
Grammatical Framework, while being an independent language instead of an EDSL, is a great example of a DSL.
The concepts of syntax and semantics are central to GF.
Each program is a *grammar*, which consists of an abstract syntax, and a set of concrete syntaxes.
The abstract syntax defines the trees that can be formed in the grammar, and the concrete syntax(es) linearize
those trees to strings, different ways in different languages.

GF is not a Turing complete programming language.
Its constructs are designed for the task of implementing multilingual grammars, not for general-purpose programming.
As for learning to design domain-specific languages, working with GF is very useful for the purpose.

### Types: read, understand and extend Haskell programs which use advanced type system features

GF itself is a typed functional language, with higher-order functions, algebraic data types, and even dependent types (in the abstract syntax).

As an example of user-defined algebraic data types, 
we define agreement as a combination of person, number and gender. We create the following datatypes:

```haskell
param Number = Sg | Pl ;
param Gender = Masc | Fem | Neut ;
param Person = P1 | P2 | P3 ;

param Agreement = Agr Person Number Gender ;
```

Then we use them in the categories of noun phrases and verb phrases;
noun phrase has agreement as an inherent feature (*dog* is singular P3,
*dogs* plural P3), and for verb phrases, choosing the right inflection form
depends on it (*dog eats* vs. *dogs eat*).
This is implemented as a table type, with values of type `Agreement` as keys,
and strings as values.

```haskell
Agr P3 Sg => "eats" ;
Agr _  _  => "eat" ;
```


In order to construct the syntax trees, the abstract syntax defines *categories* and *functions*.
The categories can be e.g. adjective, verb, noun, or for a more domain-specific grammar, e.g. 
person, date, currency. In each concrete syntax, those types are linearized as records.
All concrete syntaxes will have to implement functions on those types, such as `modify :: Adjective -> NounPhrase -> NounPhrase`,
but how the types actually are defined, is different for each language; e.g.
In Haskell, this could be modelled in different ways; `modify` could be a polymorphic function of type
`modify :: (Adjective a, NounPhrase b) => a -> b -> b`, and `Adjective` and `NounPhrase` could be type classes.

It could be also thought as a type family `Grammar` which is parameterized by a language, and the categories would be associated types;
e.g. 

```haskell
instance Grammar Finnish where
  data NounPhrase = --something with number, case, ...

instance Grammar French where
  data NounPhrase = --something with number, gender, ...
```

There are no monads in the GF language, but the Haskell source code contains a lot; both common (State, Error, IO) and custom-defined.


### Spec: use specification based development techniques

This aspect is not directly related to the GF language, but 
our project of optimizing an algorithm in the GF library can be seen as a version of specification based development.
The current algorithm is the specification, and we're 
