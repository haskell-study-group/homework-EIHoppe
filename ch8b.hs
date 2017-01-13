-- 8.7
data Optional a = Nope | Thing a

instance Eq a => Eq (Optional a) where
  Nope    == Nope    = True
  Thing x == Thing y = x == y
  _       == _       = False

data List a = Nil | Cons a (List a)

instance Eq a => Eq (List a) where
  Nil       == Nil       = True
  Cons x xs == Cons y ys = (x == y) && xs == ys
  _         == _         = False

-- 8.8
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equiv Prop Prop

type Subst = Assoc Char Bool

type Assoc k v = [(k,v)]

p1 = And (Var 'A') (Not (Var 'A'))
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')
p5 = Or (Var 'A') (Var 'A')
p6 = Or (Var 'A') (Not (Var 'A'))

find k t = head [v | (k',v) <- t, k == k']

eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equiv p q) = (eval s p <= eval s q) && (eval s q <= eval s p)

vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)

rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

isTaut p = and [eval s p | s <- substs p]

-- 8.9
data Expr = Val Int | Add Expr Expr | Mult Expr Expr

type Cont = [Op]

data Op = EVAL Expr | ADD Int | MULT Int

evalexpr (Val n)    c = exec c n
evalexpr (Add x y)  c = evalexpr x (EVAL y : c)
evalexpr (Mult x y) c = evalexpr x (EVAL y : c)

exec []           n = n
exec (EVAL y : c) n = evalexpr y (ADD n : c) 