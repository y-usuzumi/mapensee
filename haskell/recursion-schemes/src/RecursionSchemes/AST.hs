module RecursionSchemes.AST where

import Control.Monad.Fix

data Lit = StrLit String
         | IntLit Int
         | Ident String
         deriving (Show, Eq)

data Expr a = Index a a
          | Call a [a]
          | Unary String a
          | Binary a String a
          | Paren a
          | Literal Lit
          deriving (Show, Eq, Functor)

-- data Stmt = Break
--           | Continue
--           | Empty
--           | IfElse Expr [Stmt] [Stmt]
--           | Return (Maybe Expr)
--           | While Expr [Stmt]
--           | Expression Expr
--           deriving (Show, Eq)
