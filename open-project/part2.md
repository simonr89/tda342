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

As described in part I, we have worked on the Grammatical Framework package. However, instead of modifying the original random generation module, we have created our own module, based on [Duregård et al., 2012](http://dl.acm.org/citation.cfm?id=2364515). We will describe our implementation in Section n.

# Library interface

*If the library has poor documentation, this is typically done by updating it with proper (Haddock) comments for the functions and modules it exports. You can then send this to the package's maintainer. If the documentation is already in good shape, write a summary in your report.*

# Library implementation



## Expressions

## Random generation



# Analysis of our code

Our code is in the file `Enumerate.hs`. The whole GF source code, modified to include functions in Enumerate, is in github.com/inariksit/GF](https://github.com/inariksit/GF) and can be installed by `cabal install`.

A large portion of the code in `Enumerate` is taken from Koen Claessen's implementation here: [github.com/koengit/feat/Feat.hs](https://github.com/koengit/feat/blob/master/Feat.hs). The whole data type `Space a` and `Cache a` is taken from there, along with the functions `app`, `datatype`, `choice`, `card`, `size`, `index` and `depth`.
We have implemented ourselves the functions `mkSpace` and `liftS2`.

