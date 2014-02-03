module Interpreters.BigStepCBN (int) where

import Data
import DataUtil
import DataIO

int :: Program -> Expr -> Expr

int p (Atom a) =
	Atom a

int p (Ctr name args) =
	Ctr name (map (int p) args)

int p (FCall name args) =
	int p (body // zip vs args) where
		(FDef _ vs body) = fDef p name

int p (GCall gname (arg0:args)) =
	int p (body // zip (cvs ++ vs) (cargs ++ args)) where
		(Ctr cname cargs) = int p arg0
		(GDef _ (Pat _ cvs) vs body) = gDef p gname cname

int p (TestEq (x1, x2) (e1, e2)) =
	if a1==a2 then int p e1 else int p e2 where
		Atom a1 = int p x1
		Atom a2 = int p x2

int p x = error ("unexpected expression: " ++ show x)
