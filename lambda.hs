data Expr = Var String | Lam String Expr | App Expr Expr

data Value a = V a | F (Value a -> Value a)

interpret :: [(String, Value a)] -> Expr -> Value a
interpret env (Var x) = case lookup x env of
  Nothing -> error "undefined variable"
  Just v -> v
interpret env (Lam x e) = F (\v -> interpret ((x, v):env) e)
interpret env (App e1 e2) = case interpret env e1 of
  V _ -> error "not a function"
  F f -> f (interpret env e2)
