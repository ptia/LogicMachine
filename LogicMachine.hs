import Eval
import Logic

fa = Quant ForAll
te = Quant Exists

not = Not

e1 ∧ e2 = Conn And e1 e2
e1 ∨ e2 = Conn Or e1 e2
e1 → e2 = Conn Arrow e1 e2
e1 ↔ e2 = Conn DoubleArrow e1 e2

infixr 3 ∧
infixr 2 ∨
infixr 1 →
infixr 0 ↔

c = Const
v = Var
r = Rel
f = Func

e1 == e2 = Eq e1 e2

x = "x"
y = "y"
z = "z"

s1 = fa x "" $ te y "" $ f "bought" [v x, v y] ∧ f "bought" [v y, v x]
