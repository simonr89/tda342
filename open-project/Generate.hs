{-# LANGUAGE GADTs #-}
module PGF.Generate
         ( generateAll,         generateAllDepth
         , generateFrom,        generateFromDepth
         , generateRandom,      generateRandomDepth
         , generateRandomFrom,  generateRandomFromDepth
         , prove
         , mkSpace
         ) where

import Data.Map as Map (keys, lookup)
import PGF.CId
import PGF.Data
--import PGF.Macros
import PGF.TypeCheck
--import PGF.Probabilistic

--import Data.Maybe (fromMaybe)
--import qualified Data.Map as Map
--import qualified Data.IntMap as IntMap
import Control.Monad
import Control.Monad.Identity
import System.Random


------------------------------------------------------------------------------
-- The API

-- | Generates an exhaustive possibly infinite list of
-- abstract syntax expressions.
generateAll :: PGF -> Type -> [Expr]
generateAll pgf ty = generateAllDepth pgf ty Nothing

-- | A variant of 'generateAll' which also takes as argument
-- the upper limit of the depth of the generated expression.
generateAllDepth :: PGF -> Type -> Maybe Int -> [Expr]
generateAllDepth pgf ty dp = generate () pgf ty dp

-- | Generates a list of abstract syntax expressions
-- in a way similar to 'generateAll' but instead of
-- generating all instances of a given type, this
-- function uses a template. 
generateFrom :: PGF -> Expr -> [Expr]
generateFrom pgf ex = generateFromDepth pgf ex Nothing

-- | A variant of 'generateFrom' which also takes as argument
-- the upper limit of the depth of the generated subexpressions.
generateFromDepth :: PGF -> Expr -> Maybe Int -> [Expr]
generateFromDepth pgf e dp = 
  [e | (_,_,e) <- snd $ runTcM (abstract pgf)
                               (generateForMetas (prove dp) e)
                               emptyMetaStore ()]

-- | Generates an infinite list of random abstract syntax expressions.
-- This is usefull for tree bank generation which after that can be used
-- for grammar testing.
generateRandom :: RandomGen g => g -> PGF -> Type -> [Expr]
generateRandom g pgf ty = generateRandomDepth g pgf ty Nothing

-- | A variant of 'generateRandom' which also takes as argument
-- the upper limit of the depth of the generated expression.
generateRandomDepth :: RandomGen g => g -> PGF -> Type -> Maybe Int -> [Expr]
generateRandomDepth g pgf ty dp = restart g (\g -> generate (Identity g) pgf ty dp)

-- | Random generation based on template
generateRandomFrom :: RandomGen g => g -> PGF -> Expr -> [Expr]
generateRandomFrom g pgf e = generateRandomFromDepth g pgf e Nothing

-- | Random generation based on template with a limitation in the depth.
generateRandomFromDepth :: RandomGen g => g -> PGF -> Expr -> Maybe Int -> [Expr]
generateRandomFromDepth g pgf e dp = 
  restart g (\g -> [e | (_,ms,e) <- snd $ runTcM (abstract pgf)
                                                 (generateForMetas (prove dp) e)
                                                 emptyMetaStore (Identity g)])


------------------------------------------------------------------------------
-- The main generation algorithm

generate :: Selector sel => sel -> PGF -> Type -> Maybe Int -> [Expr]
generate sel pgf ty dp =
  [e | (_,ms,e) <- snd $ runTcM (abstract pgf)
                                (prove dp emptyScope (TTyp [] ty) >>= checkResolvedMetaStore emptyScope)
                                emptyMetaStore sel]

prove :: Selector sel => Maybe Int -> Scope -> TType -> TcM sel Expr
prove dp scope (TTyp env1 (DTyp hypos1 cat es1)) = do
  vs1 <- mapM (PGF.TypeCheck.eval env1) es1
  let scope' = exScope scope env1 hypos1
  (fe,TTyp env2 (DTyp hypos2 _ es2)) <- select cat scope' dp
  case dp of
    Just 0 | not (null hypos2) -> mzero
    _                          -> return ()
  (env2,args) <- mkEnv scope' env2 hypos2
  vs2 <- mapM (PGF.TypeCheck.eval env2) es2
  sequence_ [eqValue mzero suspend (scopeSize scope') v1 v2 | (v1,v2) <- zip vs1 vs2]
  es <- mapM (descend scope') args
  return (abs hypos1 (foldl EApp fe es))
  where
    suspend i c = do
      mv <- getMeta i
      case mv of
        MBound e -> c e
        MUnbound x scope tty cs -> setMeta i (MUnbound x scope tty (c:cs))

    abs []                e = e
    abs ((bt,x,ty):hypos) e = EAbs bt x (abs hypos e)

    exScope scope env []                = scope
    exScope scope env ((bt,x,ty):hypos) = 
       let env' | x /= wildCId = VGen (scopeSize scope) [] : env
                | otherwise    = env
       in exScope (addScopedVar x (TTyp env ty) scope) env' hypos

    mkEnv scope env []                = return (env,[])
    mkEnv scope env ((bt,x,ty):hypos) = do
      (env,arg) <- if x /= wildCId
                    then do i <- newMeta scope (TTyp env ty)
                            return (VMeta i (scopeEnv scope) [] : env,Right (EMeta i))
                    else return (env,Left (TTyp env ty))
      (env,args) <- mkEnv scope env hypos
      return (env,(bt,arg):args)

    descend scope (bt,arg) = do
      let dp' = fmap (flip (-) 1) dp
      e <- case arg of
             Right e  -> return e
             Left tty -> prove dp' scope tty
      e <- case bt of
             Implicit -> return (EImplArg e)
             Explicit -> return e
      return e


-- Helper function for random generation. After every
-- success we must restart the search to find sufficiently different solution.
restart :: RandomGen g => g -> (g -> [a]) -> [a]
restart g f =
  let (g1,g2) = split g
  in case f g1 of
       []     -> []
       (x:xs) -> x : restart g2 f


------------------------------------------------------------------------------
-- Selectors

instance Selector () where
  splitSelector s = (s,s)
  select cat scope dp = do
    gens <- typeGenerators scope cat
    TcM (\abstr k h -> iter k gens)
    where
      iter k []              ms s = id
      iter k ((_,e,tty):fns) ms s = k (e,tty) ms s . iter k fns ms s


instance RandomGen g => Selector (Identity g) where
  splitSelector (Identity g) = let (g1,g2) = split g
                               in (Identity g1, Identity g2)

  select cat scope dp = do
    gens <- typeGenerators scope cat
    TcM (\abstr k h -> iter k 1.0 gens)
    where
      iter k p []   ms (Identity g) = id
      iter k p gens ms (Identity g) = let (d,g')    = randomR (0.0,p) g
                                          (g1,g2)   = split g'
                                          (p',e_ty,gens') = hit d gens
                                      in k e_ty ms (Identity g1) . iter k (p-p') gens' ms (Identity g2)

      hit :: Double -> [(Double,Expr,TType)] -> (Double,(Expr,TType),[(Double,Expr,TType)])
      hit d (gen@(p,e,ty):gens)
        | d < p || null gens = (p,(e,ty),gens)
        | otherwise = let (p',e_ty',gens') = hit (d-p) gens
                      in (p',e_ty',gen:gens')

-----------------------------------------------------------------------------

data Space a where
  Empty :: Space a
  Unit  :: a -> Space a
  (:+:) :: Space a -> Space a -> Space a
  (:*:) :: Space a -> Space b -> Space (a,b)
  Map   :: (a->b)  -> Space a -> Space b
  Pay   :: Space a -> Space a
  Cache :: Cache a -> Space a -> Space a

app :: Space (a->b) -> Space a -> Space b
f `app` x = Map (uncurry ($)) (f :*: x)

--------------------------------------------------------------------------------

choice    :: [Space a] -> Space a
choice ps = foldr (:+:) Empty ps

prod        :: [Space a] -> Space a
prod []     = Empty
prod [p]    = p
prod (p:ps) = p :*: product ps

datatype    :: [Space a] -> Space a
datatype ps = cache (Pay (choice ps))

------------------------------------------------------------------------------

data Cache a
  = Memo Integer [Space a]

card' :: Cache a -> Integer
card' (Memo n _) = n

size' :: Int -> Cache a -> Space a
size' k (Memo _ ps) = ps !! k

cache :: Space a -> Space a
cache p = Cache (Memo (card p) [ cache (size k p) | k <- [0..] ]) p

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

functionsByCat :: PGF -> CId -> [CId]                 
functionsByCat pgf cat =
  case Map.lookup cat (cats (abstract pgf)) of
    Just (_,fns,_) -> map snd fns
    Nothing        -> []

functionType :: PGF -> CId -> Maybe Type
functionType pgf fun =
  case Map.lookup fun (funs (abstract pgf)) of
    Just (ty,_,_,_) -> Just ty
    Nothing         -> Nothing


mkSpace     :: PGF -> Space Expr              
mkSpace pgf =
    let abs = abstract pgf
        categories = Map.keys $ cats abs
    in datatype $ map mkSpaceOfCat categories
    where
      -- given the CId of a category, return the space of the functions that construct it 
      mkSpaceOfCat cat = choice $ map mkSpaceOfConstr $ functionsByCat pgf cat
      -- given the CId of a function, return its space
      mkSpaceOfConstr cons = case functionType pgf cons of
                               Nothing -> Empty
                               Just (DTyp hyps _ exprs) ->
                                   case hyps of
                                     [] -> Unit (EFun cons)
                                     _ -> foldr app (Unit (EApp (EFun cons))) (map (\(_,_, DTyp _ cid _) -> mkSpaceOfCat cid) hyps)
