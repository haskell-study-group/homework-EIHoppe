-- 8.1
mult Zero _     = Zero
mult _ Zero     = Zero
mult (Succ n) m = add m (mult n m)

-- 8.3
leaves Leaf a   = 1
leaves Node l r = leaves l + leaves r

balanced Leaf a   = True
balanced Node l r = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

-- 8.5
folde Expr 

-- 8.6