import Data.Char
import Data.List
import Data.List.Split
import Debug.Trace

data Expr = ForAll String Expr 
          | Exists String Expr
          | Not Expr
          | And Expr Expr
          | Or Expr Expr
          | Arrow Expr Expr
          | DoubleArrow Expr Expr
          | Const String
          | Var String
          | Func String [Expr]
          | List [Expr]
          deriving (Show)

isSep, isSym, isQuant :: Char -> Bool
isSep   = flip elem "()"
isSym   = flip elem "∀∃∧∨→↔¬,"
isQuant = flip elem "∀∃"

tokenise :: String -> [String]
tokenise ""
  = []
tokenise (c : cs)
  | isAlpha c || isQuant c = (c : w) : tokenise ws
  | isSym c || isSep c     = [c] : tokenise cs
  | c == ' '               = tokenise cs
  where
    (w, ws) = span isAlpha cs

precedence :: String -> Int
precedence ('∀' : _) = 4
precedence ('∃' : v) = 4
precedence "¬" = 4
precedence "∧" = 3
precedence "∨" = 2
precedence "→" = 1
precedence "↔" = 0
precedence "," = -1
precedence "(" = -2
precedence fun = 100

parseSym :: String -> [Expr] -> [Expr]
parseSym "¬" (a : as)          = Not a : as
parseSym "∧" (a1 : a2 : as)    = And a2 a1 : as
parseSym "∨" (a1 : a2 : as)    = Or a2 a1 : as
parseSym "→" (a1 : a2 : as)    = Arrow a2 a1 : as
parseSym "↔" (a1 : a2 : as)    = DoubleArrow a2 a1 : as
parseSym ('∀' : v) (a : as)    = ForAll v a : as
parseSym ('∃' : v) (a : as)    = Exists v a : as
parseSym "," (List l : a : as) = List (a : l) : as
parseSym "," (a1 : a2 : as)    = List [a2, a1] : as
parseSym f (List l : as)       = Func f l : as
parseSym f (a : as)            = Func f [a] : as

--Logic is right-associative
parse' :: [String] -> [String] -> [Expr]-> Expr
parse' [] ops args
  = head (foldl (flip parseSym) args ops)
parse' (t : ts) ops args
  | t == "("         = parse' ts ("(" : ops) args
  | isSym (head t)   = parse' ts (t : opsLo) (foldl (flip parseSym) args opsHi)
  | t == ")"         = parse' ts (tail opsOut) (foldl (flip parseSym) args opsIn)
  | isFunc           = parse' ts (t : opsLo) (foldl (flip parseSym) args opsHi)
  | isUpper (head t) = parse' ts ops (Const t : args)
  | isLower (head t) = parse' ts ops (Var t : args)
  where
    (opsHi, opsLo)  = span ((precedence t <) . precedence) ops
    (opsIn, opsOut) = break ("(" ==) ops
    isFunc          = isAlpha (head t) && isPrefixOf ["("] ts

parse :: String -> Expr
parse s = parse' (tokenise s) [] []

--Some examples
ex1 = "∃x∀y((human(y) ∧ ¬eq(Clyde, y)) → bought(y, x))"
