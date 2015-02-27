# AFP -- Assignment 3

Package chosen: http://hackage.haskell.org/package/gf

Grammatical Framework, a programming language for multilingual grammar applications.

```
$ gf LangEng.gf LangSwe.gf

Lang> parse -lang=Eng -cat=Cl "I am an apple" 
PredVP (UsePron i_Pron) (UseComp (CompCN (UseN apple_N)))
```

It parses strings into abstract syntax trees, and using that as an interlingua, can translate between languages.

```
Lang> parse -lang=Eng "I am an apple" | linearize -lang=Swe
jag är ett äpple
```

It can also generate random trees:

```
Lang> gr -tr | l
PhrUtt (PConjConj and_Conj) (UttCN (AdvCN (UseN hand_N) today_Adv)) please_Voc

and hand today please
och hand idag tack
```

## How is this related to learning outcomes

...

## Plan A: optimize the algorithm for random generation
http://dl.acm.org/citation.cfm?id=2364515

## Plan B: document Haskell bindings for C runtime
there's plenty to document
