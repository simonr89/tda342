{-# LANGUAGE GADTs #-}
module FEAT.Feat where

infixl 1 `app`

--------------------------------------------------------------------------------

data Space a where
  Empty :: Space a
  Unit  :: a -> Space a
  (:+:) :: Space a -> Space a -> Space a
  (:*:) :: Space a -> Space b -> Space (a,b)
  Map   :: (a->b)  -> Space a -> Space b
  Pay   :: Space a -> Space a
  Cache :: Cache a -> Space a -> Space a

--------------------------------------------------------------------------------

app :: Space (a->b) -> Space a -> Space b
f `app` x = Map (uncurry ($)) (f :*: x)

--------------------------------------------------------------------------------

choice :: [Space a] -> Space a
choice ps = foldr (:+:) Empty ps

datatype :: [Space a] -> Space a
datatype ps = cache (Pay (choice ps))

--------------------------------------------------------------------------------

bool :: Space Bool
bool =
  datatype
  [ Unit False
  , Unit True
  ]

list :: Space a -> Space [a]
list x = list_x
 where
  list_x =
    datatype
    [ Unit []
    , Unit (:) `app` x `app` list_x
    ]

--------------------------------------------------------------------------------

card :: Space a -> Integer
card Empty       = 0
card (Unit _)    = 1
card (p :+: q)   = card p + card q
card (p :*: q)   = card p * card q
card (Map _ p)   = card p
card (Pay p)     = card p
card (Cache c _) = card' c

--------------------------------------------------------------------------------

size :: Int -> Space a -> Space a
size 0 (Unit x)        = Unit x
size k (p :+: q)       = size k p :+: size k q
size k (p :*: q)       = choice [ size i p :*: size (k-i) q | i <- [0..k] ]
size k (Map f p)       = Map f (size k p)
size k (Pay p) | k > 0 = Pay (size (k-1) p)
size k (Cache c _)     = size' k c
size _ _               = Empty

--------------------------------------------------------------------------------

data Cache a
  = Memo Integer [Space a]

card' :: Cache a -> Integer
card' (Memo n _) = n

size' :: Int -> Cache a -> Space a
size' k (Memo _ ps) = ps !! k

cache :: Space a -> Space a
cache p = Cache (Memo (card p) [ cache (size k p) | k <- [0..] ]) p

--------------------------------------------------------------------------------

index :: Space a -> Integer -> a
index (Unit x)    0 = x
index (p :+: q)   i
  | i < n           = index p i
  | otherwise       = index q (i-n)                              where n = card p
index (p :*: q)   i = (index p (i `mod` n), index q (i `div` n)) where n = card p
index (Map f p)   i = f (index p i)
index (Pay p)     i = index p i
index (Cache _ p) i = index p i

--------------------------------------------------------------------------------

depth :: Int -> Space a -> Space a
depth d (Unit x)        = Unit x
depth d (p :+: q)       = depth d p :+: depth d q
depth d (p :*: q)       = depth d p :*: depth d q
depth d (Pay p) | d > 0 = Pay (depth (d-1) p)
depth d (Map f p)       = Map f (depth d p)
depth d (Cache _ p)     = cache (depth d p)
depth _ _               = Empty

--------------------------------------------------------------------------------

data Nat = Z | S Nat

instance Show Nat where
  show n = show (val n)
   where
    val Z     = 0
    val (S n) = 1+val n

nat :: Space Nat
nat = datatype [ Unit Z, Unit S `app` nat ]

--------------------------------------------------------------------------------

data N10 = N10 Int

instance Show N10 where
  show (N10 n) = show n

n10 :: Space N10
n10 = datatype [ Unit (N10 i) | i <- [1..10] ]

--------------------------------------------------------------------------------


