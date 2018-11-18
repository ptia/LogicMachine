module Logic where

-- Syntax
data Connective = And | Or | Arrow | DoubleArrow deriving (Show, Eq)
data Quantifier = ForAll | Exists deriving (Show)
data Expr = Quant Quantifier String Expr 
          | Conn Connective Expr Expr
          | Not Expr
          | Const String
          | Var String
          | Rel String [Expr]
          | Func String [Expr]
          | List [Expr]
          deriving (Show)

-- Semantics
type Object     = String
type Binding    = (String, Object)
type Relation   = (String, [Object] -> Bool)
type Function   = (String, [Object] -> Object)
type Assignment = [Binding]
data Model      = Model {objs :: [Object],
                         consts :: [Binding],
                         rels :: [Relation],
                         funcs :: [Function]}
