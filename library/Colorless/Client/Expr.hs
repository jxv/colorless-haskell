module Colorless.Client.Expr
  (
  ) where

import Colorless.Types

-- q1 :: Expr () a -> Query a
-- q2 :: Expr a' a -> (a' -> Expr b' b) -> Query (a, b)
-- q3 :: Expr a' a -> (a' -> Expr b' b) -> (b' -> Expr () c) -> Query (a, b, c)
-- q4 :: Expr a' a -> (a' -> Expr b' b) -> (b' -> Expr c' c) -> (c' -> Expr () d) -> Query (a, b, c, d)

data Expr a' a
  = Ref Symbol
  | If (Expr a' Bool) (Expr a' a) (Expr a' a)
  | Def Symbol (Expr a' a)
  | Begin (Expr a' (Expr a' a))
  | List [Expr a' a]
  | Val a
  | Fn [Symbol] a
  deriving (Show, Eq)

if' :: Expr a' Bool -> Expr a' a -> Expr a' a -> Expr a' a
if' = If

def :: Symbol -> Expr a' a -> Expr a' a
def = Def

fn0 :: Expr a' a -> Expr a' (() -> Expr a' a)
fn0 f = Fn [] (\() -> f)

fn1 :: Symbol -> (Expr a' t1 -> Expr a' a) -> Expr a' (Expr a' t1 -> Expr a' a)
fn1 t1 f = Fn [t1] $ \_ -> f (Ref t1)

fn2 :: Symbol -> Symbol -> ((Expr a' t1, Expr a' t2) -> Expr a' a) -> Expr a' ((Expr a' t1, Expr a' t2) -> Expr a' a)
fn2 t1 t2 f = Fn [t1,t2] $ \_ -> f (Ref t1, Ref t1)

fn3 :: Symbol -> Symbol -> Symbol -> ((Expr a' t1, Expr a' t2, Expr a' t3) -> Expr a' a) -> Expr a' ((Expr a' t1, Expr a' t2, Expr a' t3) -> Expr a' a)
fn3 t1 t2 t3 f = Fn [t1,t2,t3] $ \_ -> f (Ref t2, Ref t1, Ref t3)

call :: Expr a' (b -> Expr a' a) -> b -> Expr a' a
call (Fn _ f) x = f x
call _ _ = error "should never reach"
