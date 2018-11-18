module Eval where

import Logic
import Data.Maybe
import Data.Either

conns = [(And, (&&)),
         (Or, (||)),
         (Arrow, (\ b1 b2 -> not b1 || b2)),
         (DoubleArrow, (==))]

eval :: Expr -> Model -> Assignment -> Either Bool Object

eval (Quant ForAll v e) m h
  = Left $ and $ lefts [eval e m ((v, o) : h) | o <- objs m]
eval (Quant Exists v e) m h
  = Left $ or $ lefts [eval e m ((v, o) : h) | o <- objs m]

eval (Conn c e1 e2) m h
  = Left $ fromJust (lookup c conns) b1 b2
  where
    Left b1 = eval e1 m h
    Left b2 = eval e2 m h

eval (Not e) m h
  = Left $ not b
  where
    Left b = eval e m h

eval (Const c) m h
  = Right $ fromJust (lookup c (consts m))

eval (Var v) m h
  = Right $ fromJust (lookup v h)

eval (Rel r args) m h
  = Left $ fromJust (lookup r (rels m)) args'
  where
    args' = rights (map (eval' m h) args)
  
eval (Func f args) m h
  = Right $ fromJust (lookup f (funcs m)) args'
  where
    args' = rights (map (eval' m h) args)

eval' :: Model -> Assignment -> Expr -> Either Bool Object
eval' m h e = eval e m h

-- Some examples
bought ["Tony", "308"] = True
bought ["308", "Clyde"] = True
bought ["Clyde", "Tony"] = True
bought ["Tony", "Tony"] = True
bought [_, _] = False
m = Model ["Tony", "Clyde", "308"] [("Daddy", "Tony"), ("Clyde", "Clyde"), ("Room", "308")] [("bought", bought)] []
s1 = Quant Exists "x" (Rel "bought" [Const "Daddy",Var "x"])
s2 = Quant ForAll "x" (Rel "bought" [Const "Daddy",Var "x"])
s3 = Quant ForAll "x" (Conn Or (Rel "bought" [Const "Daddy",Var "x"]) (Rel "bought" [Var "x",Const "Daddy"]))
