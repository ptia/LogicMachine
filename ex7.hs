import Logic
import Parser
import Eval
import LogicMachine

maxi = 20

m = Model (zip (map show [0..maxi]) (repeat ""))
          (zip (map show [0..maxi]) (map show [0..maxi]))
          [("lt", \[a1, a2] -> (read a1 :: Integer) < (read a2 :: Integer))]
          [("add", \[a1, a2] -> show $ (read a1 :: Integer) + (read a2 :: Integer))
          ,("mul", \[a1, a2] -> show $ (read a1 :: Integer) * (read a2 :: Integer))]

--x is a multiple of 3
f1 = te "u" "" (v x === f "mul" [v "u", c "3"] )
--x is prime
f2 = parse "¬∃u(¬(u=x)∧¬(u=1)∧∃v(x=mul(u,v)))"
--sentence: Goldbachs conjecture
f3 = parse "∀x(lt(2,x) ∧ ∃u(x=mul(2,u)) → ∃p1∃p2( ¬∃u(¬(u=p1)∧¬(u=1)∧∃v(p1=mul(u,v))) ∧ ¬(p1=1) ∧¬∃u(¬(u=p2)∧¬(u=1)∧∃v(p2=mul(u,v))) ∧ ¬(p2=1) ∧ x = add(p1,p2)))"
