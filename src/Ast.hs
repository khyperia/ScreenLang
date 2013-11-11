module Ast where

import LLVM.General.AST.Type

type Block = [Statement]

data Attributes = DllImport
                | StdCall
        deriving(Eq, Show)

data Statement = IfStatement Expression Block Block
               | WhileStatement Expression Block
               | ExprStatement Expression
               | Return (Maybe Expression)
               | Function [Attributes] Type String [(Type, String)] (Maybe Block)
        deriving(Eq, Show)

data Expression = Addition Expression Expression
                | Subtraction Expression Expression
                | Multiplication Expression Expression
                | Division Expression Expression
                | ShiftLeft Expression Expression
                | ShiftRight Expression Expression
                | Or Expression Expression
                | And Expression Expression
                | Negation Expression
                | CreateTuple Type [Expression]
                | MethodCall Expression [Expression]
                | Assignment Expression Expression
                | Identifier String
                | ConstantInteger Integer
                | ConstantBoolean Bool
        deriving(Eq, Show)
