module Interpreters.SmallStepCBN (int, intCount) where

import Data
import DataUtil
import DataIO

-- small-step interpreter for call-by-name semantics

int :: Program -> Expr -> Expr
int p e = until isValue (intStep p) e

intCount :: Program -> Expr -> (Expr, Integer)
intCount p e = until t f (e, 0) where
	t (e, n) = isValue e
	f (e, n) = (intStep p e, n + 1)

intStep :: Program -> Expr -> Expr
intStep p (Ctr name args) =
	Ctr name (values ++ (intStep p x : xs)) where
		(values, x : xs) = span isValue args

intStep p (FCall name args) =
	body // zip vs args where
		(FDef _ vs body) = fDef p name

intStep p (GCall gname (Ctr cname cargs : args)) =
	body // zip (cvs ++ vs) (cargs ++ args) where
		(GDef _ (Pat _ cvs) vs body) = gDef p gname cname

intStep p (GCall gname (e:es)) =
	(GCall gname (intStep p e : es))

intStep p (TestEq (Atom a1, Atom a2) (e1, e2)) =
	if a1==a2 then e1 else e2

intStep p (TestEq (Atom a1, y) (e1, e2)) =
	(TestEq (Atom a1, intStep p y) (e1, e2))

intStep p (TestEq (x, y) (e1, e2)) =
	(TestEq (intStep p x, y) (e1, e2))

intStep p x = error ("unexpected expression: " ++ show x)
