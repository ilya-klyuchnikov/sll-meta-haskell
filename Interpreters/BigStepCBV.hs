-- naive version
module Interpreters.BigStepCBV (int) where

import Data
import DataUtil
import DataIO

int :: Program -> Expr -> Expr

int p e = int' p (forceArgs p e)

-- the main logic of intepretatoin
int' :: Program -> Expr -> Expr
int' p (Atom a) =
	Atom a

int' p (Ctr name args) | True <- and (map isValue args) =
	Ctr name args

int' p (FCall name args) | True <- and (map isValue args) =
	int p (body // zip vs args) where
		(FDef _ vs body) = fDef p name

int' p (GCall gname (arg0:args)) | True <- and (map isValue args) =
	int p (body // zip (cvs ++ vs) (cargs ++ args)) where
		(Ctr cname cargs) = int p arg0
		(GDef _ (Pat _ cvs) vs body) = gDef p gname cname

int' p (TestEq (x1, x2) (e1, e2)) | True <- and (map isValue [x1, x2]) =
	if a1==a2 then int p e1 else int p e2 where
		Atom a1 = int p x1
		Atom a2 = int p x2

int' p x = error ("unexpected expression: " ++ show x)

-- forces evaluation of arguments
-- it is lazy forcement!
forceArgs :: Program -> Expr -> Expr
forceArgs p (Atom a) =
	Atom a

forceArgs p (Ctr name args) =
	Ctr name (map (int p) args)

forceArgs p (FCall name args) =
	FCall name (map (int p) args)

forceArgs p (GCall name args) =
	GCall name (map (int p) args)

forceArgs p (TestEq (x1, x2) (e1, e2)) =
	TestEq (int p x1, int p x2) (e1, e2)
