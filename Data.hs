module Data where

type Name = String
data Expr = Var Name [Expr]
          | Atom Char
          | Ctr Name [Expr] 
          | TestEq (Expr, Expr) (Expr, Expr)
          | FCall Name [Expr] | GCall Name [Expr] 
            deriving (Eq, Ord)

data Pat = Pat Name [Name] deriving (Eq)
data GDef = GDef Name Pat [Name] Expr deriving (Eq)
data FDef = FDef Name [Name] Expr deriving (Eq)
data Program = Program [FDef] [GDef] deriving (Eq)

type Renaming = [(Name, Name)]
type Subst a = [Binding a]
type NameSupply = [Name]
type Binding a = (Name, a)

type Conf = Expr
type Value = Expr
type Task = (Conf, Program)
type Env = [(Name, Value)]

-- TestResult is used for construction of evaluation trace
data TestResult = CtrMatch Pat | TestRes Bool
data Step a = Transient (Maybe TestResult) a | Variants [(Subst a, a)]
            | Stop a | Decompose ([a] -> a) [a]
data Edge a = ETransient (Maybe TestResult) (Graph a) | EVariants [(Subst a, Graph a)] 
            | EDecompose ([a] -> a) [Graph a] | EFold (Graph a) Renaming

data Graph a = Node a (Edge a) | Leaf a
type Tree a = Graph a
type Node a = Tree a

type Machine a = a -> Step a