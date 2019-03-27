{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}

module Main where


import Control.Monad.Trans.Maybe

import Type.Reflection
import Data.Bool
import Data.List
import Data.Maybe
import Data.SCargot
import Data.SCargot.Repr.Basic
import qualified Data.SCargot.Repr.Basic as DSRB
import Data.Text (Text, pack, unpack)

import Data.String.Conversions

-- import Data.Generics (Data, Typeable, mkQ, mkT, everything, everywhere)
-- https://github.com/fsestini/mu-kanren/blob/master/src/MuKanren.hs sample is work
type Var = Int

data Atom where
  AVar :: Var -> Atom
  AData :: (Eq a, Show a, Typeable a) => a -> Atom
deriving instance Show Atom

(~=) :: (Eq a, Typeable a, Typeable b) => a -> b -> Maybe Bool
x ~= y = fmap (\HRefl -> x == y) (eqTypeRep (typeOf x) (typeOf y))

type Term = SExpr Atom 

pattern TPair a t = a ::: t
pattern TVar v = A (AVar v)
pattern TData d = A (AData d)
pattern Nil = SNil

nil :: Term
nil = SNil

printAtom :: Atom -> Text
printAtom (AVar v) = pack ("#" ++ show v)
printAtom (AData d) = pack (show d)

printTerm :: Term -> String
printTerm =
  unpack . encodeOne (setIndentStrategy (const Align) $ basicPrint printAtom)

num :: Int -> Term
num = TData

nums :: [Int] -> Term
nums = list . fmap num

list :: [Term] -> Term
list = foldr (:::) SNil  

type Sub = [(Var, Term)]
type State = (Sub, Int)
type Res = [Maybe State]
type Goal = State -> Res





walk :: Term -> Sub -> Term
walk t@(TVar v) s = maybe t (flip walk s) (lookup v s)
walk t _ = t


unify :: Term -> Term ->Sub -> Maybe Sub
unify u v s = aux (walk u s) (walk v s)                       -- some situations, u is key and v is value
  where 
    aux :: Term -> Term -> Maybe Sub
    aux (TVar u) (TVar v) | u == v = Just s                   -- if their are the same logic variable, we just return the substitude enviroment
    aux (TVar u) v = Just ((u, v) : s)                        -- append the key value to the enviroment
    aux u (TVar v) = Just ((v, u) : s)                        -- append the key value to the enviroment
    aux DSRB.Nil DSRB.Nil = Just s
    aux (TPair u v) (TPair u' v') = aux u u' >>= unify v v'   -- all unify should be sucess!
    aux (TData x) (TData y) = x ~= y >>= bool Nothing (Just s)-- if u and v are data, first we check if their type are equal , then we check if their value are equal.
    aux _ _ = Nothing

(===) :: Term -> Term -> Goal
(u === v) st = maybe []                                       -- if unify fails we return empty list
                     (\unifyRes -> pure (Just (unifyRes, snd st))) -- if unify success, we pass the unify result to this function, and return new state
                     (unify u v (fst st))                     -- unify the key and value



callFresh :: (Var ->Goal) -> Goal
callFresh f = \sc -> f (snd sc) (fst sc, (snd sc) + 1)
{-- 
f is an function with an question argument , it's body will use === function to do some disj or conj.
the question argument will be substide by the logical variable ( f (snd sc) ), the snd sc , is the number of the logical var.
fst sc is the substute enviroment
snd sc is the current logical variable index
--}


interleave :: [a] -> [a] -> [a]
interleave xs ys = (concat .transpose) [xs, ys]

{-- 
the transpose and concat works like as:
transpose [[1,2,3,4], [5,6,7,8]]
[[1,5],[2,6],[3,7],[4,8]]

concat [[1,5],[2,6],[3,7],[4,8]]
[1,5,2,6,3,7,4,8]
--}


conj, disj :: Goal -> Goal -> Goal
disj g1 g2  = \sc ->  interleave (g1 sc) (g2 sc)                      -- disjunction
conj g1 g2  = \sc ->  runMaybeT $ MaybeT (g1 sc) >>= MaybeT . g2      -- conjunction, it will return nothing once there is one unify failure.


emptyState :: State
emptyState = ([], 0)


delay = (Nothing : )


-- fives :: Term -> Goal
-- fives x = (x === num 5)
-- fives (num 5) (emptyState )   
-- if we call like this , the return value is [Just ([],0)], it says that there is an answer !!
-- why Just [], 0, because when we unify 5 and 5 (which type is TData), we simply return the empty substitute enviroment.



fives :: Term -> Goal
fives x = disj (x === num 5) (delay . fives x) 
-- fives (num 5) (emptyState )   
-- if we call like this, the return value is [Just ([],0),Nothing,Just ([],0),Nothing,Just ([],0),Nothing,Just



trunc :: Int -> [Maybe State] -> [Maybe State]
trunc _ [] = []
trunc m (Just x : xs) = Just x : trunc m xs
trunc m (Nothing : xs) = if m == 0 then [] else Nothing : trunc (m - 1) xs


mytake :: Int -> Goal -> [ State]
mytake n g =take n $ catMaybes $ (trunc n) (g emptyState)
--  mytake 5 $ fives (num 5)
--  if we call like this, we can get five answer！ like [([],0),([],0),([],0),([],0),([],0)]
--  the answer says that, we can always unify successfully with num 5 and num 5. we unified 5 times,and all 5 time succeed.

mytake2 :: Int -> Goal -> [ State]
mytake2 n g =take n $ catMaybes $  (g emptyState)
-- if we call mytake2 5 $ fives (num 5) ,  the call will loop infinity
-- because catMaybes with lots of nothing will be [], so we'd better use trunc to limit the recursive calls.
-- so the cleaver way is like as :

takeLimit :: Int -> Maybe Int -> [Maybe State] -> [State]
takeLimit n m res= take n $ catMaybes $ (maybe id trunc m) $ res

-- takeLimit 5 (Just 10) $ fives (num 5) emptyState


ppSub :: Sub -> String
ppSub sc = "[" ++ f sc ++ "]"
  where f = concat . intersperse ", " .
             fmap (\(v,t) -> "(#" ++ show v ++ ", " ++ printTerm t ++ ")")

ppState :: State -> String
ppState (s,c) = "(" ++ ppSub s ++ ", " ++ show c ++ ")"

-- fmap ppState $ takeLimit 5 (Just 10) $ fives (num 5) emptyState


takeS :: Int -> Goal -> IO ()
takeS n g = mapM_ putStrLn ( fmap ppState . takeLimit n Nothing $ g emptyState)

takeS' :: Int -> Int -> Goal -> IO ()
takeS' n m g = mapM_ putStrLn ( fmap ppState . takeLimit n (Just m) $ g emptyState)

-- takeS' 1 10 (fives (num 3)) will print Nothing, because catMaybes lots of Nothing is []
-- takeS 1  (fives (num 3))  will loop forever, because  catMaybes lots of Nothing is [], and take 1 answer will wait forever.






data Expr = 
  Atom Integer
  | Add Expr Expr
  | Mul Expr Expr deriving Show


translate :: Expr -> SExpr (Text)  -- translate haskell adt to lisp !
translate e = case e of
  Atom t        -> SAtom(pack $ show  t)
  Add rator rand ->  L [A "+", translate rator, translate rand]
  Mul rator rand ->  L [A "*", translate rator, translate rand]


ppExpr :: Expr -> Text
ppExpr e = encode  (setMaxWidth 10 $ basicPrint id) [translate e]





n2i i = i :: Integer;

main :: IO ()
main = do
  -- putStrLn $ cs $ ppExpr a
  -- putStrLn $ show a
  putStrLn $ printTerm $ walk (TVar 4) [(3, (TVar 2)), (4, (TVar 1))]
  putStrLn $ printTerm $ walk (TVar 4) [(3, (TVar 2)), (4, (TData  (n2i 2)))]
  where 
    a = Add (Add (Atom 1) (Atom 2)) (Mul (Atom 3) (Atom 4))









