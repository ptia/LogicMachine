module Logic where

-- Syntax
data Connective = And | Or | Arrow | DoubleArrow deriving (Show, Eq)
data Quantifier = ForAll | Exists deriving (Show)
data Expr = Quant Quantifier String String Expr 
          | Conn Connective Expr Expr
          | Not Expr
          | Const String
          | Var String
          | Rel String [Expr]
          | Func String [Expr]
          | Eq Expr Expr
          | List [Expr]
          deriving (Show)

-- Semantics
type Object     = String
type Sort       = String
type Binding    = (String, Object)
type Relation   = (String, [Object] -> Bool)
type Function   = (String, [Object] -> Object)
type Assignment = [Binding]
data Model      = Model {objs   :: [(Object, Sort)],
                         consts :: [Binding],
                         rels   :: [Relation],
                         funcs  :: [Function]}

instance Show Model where
  show (Model objs consts rels funcs)
    = "Model {objs = " ++ show objs
      ++ ", consts = " ++ show consts
      ++ ", rels = " ++ show (map fst rels)
      ++ ", funcs = " ++ show (map fst funcs) 
      ++ "}"
  showsPrec
    = undefined
