module Data where

-- Common data structures

-- SLL, Simple Lazy Language
type Name = String
-- Var possibly contains restrictions (a list of atoms)
data Expr = Var Name [Expr]
          | Atom Char
          | Ctr Name [Expr]
          | TestEq (Expr, Expr) (Expr, Expr)
          | FCall Name [Expr]
          | GCall Name [Expr]
            deriving (Eq, Ord)

data Pat = Pat Name [Name] deriving (Eq)
data GDef = GDef Name Pat [Name] Expr deriving (Eq)
data FDef = FDef Name [Name] Expr deriving (Eq)
data Program = Program [FDef] [GDef] deriving (Eq)

-- Syntax boilerplate
type Renaming = [(Name, Name)]
type Binding a = (Name, a)
type Subst a = [Binding a]
type NameSupply = [Name]

type Task = (Expr, Program)
type Env = [(Name, Expr)]

-- TestResult is used for construction of evaluation trace
data TestResult = CtrMatch Pat
                | TestRes Bool

-- Evaluation step
data Step a = Transient (Maybe TestResult) a
            | Variants [(Subst a, a)]
            | Stop a
            | Decompose Name [a]

-- Egde in evaluation tree, edge in tree of configuraions ...
data Edge a = ETransient (Maybe TestResult) (Tree a)
            | EVariants [(Subst a, Tree a)]
            | EDecompose Name [Tree a]

-- Evaluation tree, tree of configurations ...
data Tree a = Node a (Edge a) | Leaf a

-- Machine = evaluator
type Machine a = a -> Step a
