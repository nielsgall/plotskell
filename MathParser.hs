{-# LANGAUGE OverloadedStrings #-}

module Main where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language (haskell)
import Control.Monad (liftM, liftM2)
import qualified Data.Map as M

data Expr a = Num  Double
            | Var  String
            | Neg  (Expr a)
            | Add  (Expr a) (Expr a)
            | Sub  (Expr a) (Expr a)
            | Mul  (Expr a) (Expr a)
            | Div  (Expr a) (Expr a)
            | Sqrt (Expr a)
            | Exp  (Expr a)
            | Log  (Expr a)
            | Sin  (Expr a)
            | Cos  (Expr a)
            | Tan  (Expr a)
            deriving Show

expr    =  buildExpressionParser table term
       <?> "expression"

term    =  parens haskell expr 
       <|> Var <$> identifier haskell
       <|> Num <$> realToFrac <$> integer haskell
       <|> Num <$> float haskell
       <?> "simple expression"

table   = [ [prefix "sin"  Sin ]
          , [prefix "cos"  Cos ]
          , [prefix "tan"  Tan ]
          , [prefix "exp"  Exp ]
          , [prefix "log"  Log ]
          , [prefix "sqrt" Sqrt]
          , [prefix "-" Neg]
          , [binary "*" Mul AssocLeft, binary "/" Div AssocLeft]
          , [binary "+" Add AssocLeft, binary "-" Sub AssocLeft]
          ]

binary  name fun = Infix $ reservedOp haskell name >> return fun
prefix  name fun = Prefix (do{ reservedOp haskell name; return fun })
postfix name fun = Postfix (do{ reservedOp haskell name; return fun })

evaluate :: M.Map String Double -> (Expr Double) -> Maybe Double
evaluate def ast =
    case ast of
        Num        num -> Just num
        Var        var -> M.lookup var def
        Neg       ast1 -> liftM  (negate) (evaluate def $ ast1)
        Add  ast1 ast2 -> liftM2 (+)      (evaluate def $ ast1) (evaluate def $ ast2)
        Sub  ast1 ast2 -> liftM2 (-)      (evaluate def $ ast1) (evaluate def $ ast2)
        Mul  ast1 ast2 -> liftM2 (*)      (evaluate def $ ast1) (evaluate def $ ast2)
        Div  ast1 ast2 -> liftM2 (/)      (evaluate def $ ast1) (evaluate def $ ast2)
        Sqrt      ast1 -> liftM  (** 0.5) (evaluate def $ ast1)
        Exp       ast1 -> liftM  (exp)    (evaluate def $ ast1)
        Log       ast1 -> liftM  (log)    (evaluate def $ ast1)
        Sin       ast1 -> liftM  (sin)    (evaluate def $ ast1)
        Cos       ast1 -> liftM  (cos)    (evaluate def $ ast1)
        Tan       ast1 -> liftM  (tan)    (evaluate def $ ast1)
        _              -> Nothing

