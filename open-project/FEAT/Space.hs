module FEAT.Space where

import Data.List( intersperse )

infixl 1 `app`

--------------------------------------------------------------------------------

-- a sequence Seq is like a (finite) list. It is a pair of the size of the
-- list, and the function we can use to index in it

type Seq a = (Integer, Integer -> a)

nil :: Seq a
nil = (0, error "nothing here!")

single :: a -> Seq a
single x = (1, \0 -> x)

(+!+) :: Seq a -> Seq a -> Seq a
(n1,h1) +!+ (n2,h2) = (n1+n2, \i -> if i < n1 then h1 i else h2 (i-n1))

(*!*) :: Seq a -> Seq b -> Seq (a,b)
(n1,h1) *!* (n2,h2) = (n1*n2, \i -> (h1 (i `mod` n1), h2 (i `div` n1)))

showSeq :: Show a => Seq a -> String
showSeq (n,h) = show [ h i | i <- [0..n-1] ]

--------------------------------------------------------------------------------

-- a space is a collection of sequences, one such sequence for each size. We can
-- use the pay function to control size.

type Space a = [Seq a]

empty :: Space a
empty = []

unit :: a -> Space a
unit x = [single x]

(+++) :: Space a -> Space a -> Space a
xs     +++ []     = xs
[]     +++ ys     = ys
(x:xs) +++ (y:ys) = (x +!+ y) : (xs +++ ys)

(***) :: Space a -> Space b -> Space (a,b)
[]     *** w = []
(x:xs) *** w = map (x *!*) w ++> (xs *** w)
 where
  -- xs ++> ys === xs +++ pay ys
  []     ++> ys = pay ys
  (x:xs) ++> ys = x : (xs +++ ys)
  
pay :: Space a -> Space a
pay v = nil : v

--------------------------------------------------------------------------------

maps :: (a->b) -> Space a -> Space b
maps f v = [ (n, f . h) | (n,h) <- v ]

app :: Space (a->b) -> Space a -> Space b
f `app` x = maps (uncurry ($)) (f *** x)

--------------------------------------------------------------------------------

bool :: Space Bool
bool = pay (unit False +++ unit True)

list :: Space a -> Space [a]
list x = list_x
 where
  list_x = pay ( unit []
             +++ (unit (:) `app` x `app` list_x)
               )

--------------------------------------------------------------------------------

data Exp
  = Var Int
  | App Exp Exp
  | Lam Exp
 deriving ( Eq, Show )

expr :: Int -> Space Exp
expr k = expk
 where
  expk =
    pay ( foldr (+++) empty [ unit (Var i) | i <- [1..k] ]
      +++ (unit App `app` expk `app` expk)
      +++ (unit Lam `app` expr (k+1))
        )

--------------------------------------------------------------------------------

showSpace :: Show a => Space a -> String
showSpace v = "{" ++ concat (intersperse "," [ show s ++ ":" ++ showSeq p | (p@(n,_),s) <- v `zip` [0..], n > 0 ]) ++ "}"

--------------------------------------------------------------------------------

