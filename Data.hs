module Data where

type Name = String
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

type Renaming = [(Name, Name)]
type Binding a = (Name, a)
type Subst a = [Binding a]
type NameSupply = [Name]

type Task = (Expr, Program)
type Env = [(Name, Expr)]

-- TestResult is used for construction of evaluation trace
data TestResult = CtrMatch Pat
                | TestRes Bool  

data Step a = Transient (Maybe TestResult) a 
            | Variants [(Subst a, a)]
            | Stop a 
            | Decompose Name [a]

data Edge a = ETransient (Maybe TestResult) (Tree a) 
            | EVariants [(Subst a, Tree a)] 
            | EDecompose Name [Tree a]

data Tree a = Node a (Edge a) | Leaf a

type Machine a = a -> Step a
