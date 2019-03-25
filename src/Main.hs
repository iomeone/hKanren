{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}

module Main where


import Type.Reflection
import Data.SCargot
import Data.SCargot.Repr.Basic
import Data.Text (Text, pack, unpack)

import Data.String.Conversions

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


main :: IO ()
main = do
  putStrLn $ cs $ ppExpr a
  putStrLn $ show a
  putStrLn "hello world"
  where 
    a = Add (Add (Atom 1) (Atom 2)) (Mul (Atom 3) (Atom 4))









