module Parser where

import Logic
import Data.Char
import Data.List
import Data.List.Split
import Debug.Trace

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
parseSym "∧" (a1 : a2 : as)    = Conn And a2 a1 : as
parseSym "∨" (a1 : a2 : as)    = Conn Or a2 a1 : as
parseSym "→" (a1 : a2 : as)    = Conn Arrow a2 a1 : as
parseSym "↔" (a1 : a2 : as)    = Conn DoubleArrow a2 a1 : as
parseSym ('∀' : v) (a : as)    = Quant ForAll v a : as
parseSym ('∃' : v) (a : as)    = Quant Exists v a : as
parseSym "," (List l : a : as) = List (a : l) : as
parseSym "," (a1 : a2 : as)    = List [a2, a1] : as
parseSym "(" _                 = error "Unmatched ("
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

--Turns top-level funcs into rels
parse'' :: Expr -> Expr
parse'' (Quant q v e)  = Quant q v (parse'' e)
parse'' (Conn c e1 e2) = Conn c (parse'' e1) (parse'' e2)
parse'' (Not e)        = Not (parse'' e)
parse'' (Const c)      = Const c
parse'' (Var v)        = Var v
parse'' (Func f args)  = Rel f args

parse :: String -> Expr
parse s = parse'' (parse' (tokenise s) [] [])

--Some examples
ex1 = "∃x∀y((human(y) ∧ ¬eq(Clyde, y)) → bought(y, x))"
