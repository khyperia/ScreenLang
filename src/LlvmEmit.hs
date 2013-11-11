module LlvmEmit (emit) where

import Ast
import LLVM.General.AST as AST
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as Const
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Linkage as Linkage
import Control.Monad.Writer
import Control.Monad.State
import Data.Word

data EmitState = EmitState { label :: Word, globals :: [Global] }

type Scope = [(Type, String)]

type StateWriter t = StateT EmitState (Writer [t])

wordSize :: Word32
wordSize = 32

--parseType :: String -> Type
--parseType ('p':rest) = PointerType (parseType rest) (AddrSpace.AddrSpace 0)
--parseType ('i':num) = IntegerType (read num)
--parseType ('f':num) = FloatingPointType (read num) IEEE
--parseType "void" = VoidType
--parseType t = error ("Unknown type " ++ t)

genLabel :: (Monad m) => StateT EmitState m Name
genLabel = do
        st <- get
        put (st{ label = label st + 1 })
        return (UnName (label st))

binOp :: Scope -> Expression -> Expression -> (Bool -> Bool -> Operand -> Operand -> InstructionMetadata -> Instruction) ->  StateWriter (Named Instruction) Operand
binOp scope leftExpr rightExpr con = do
        left <- genExpr scope leftExpr
        right <- genExpr scope rightExpr
        labelInst <- genLabel
        let instruction = labelInst := con False False left right []
        tell [instruction]
        return (LocalReference labelInst)

genExpr :: Scope -> Expression -> StateWriter (Named Instruction) Operand
genExpr scope (Identifier s) = return (if s `elem` map snd scope then LocalReference (Name s) else ConstantOperand (Const.GlobalReference (Name s)))
genExpr _ (ConstantInteger val) = return (ConstantOperand (Const.Int wordSize val))
genExpr _ (ConstantBoolean val) = return (ConstantOperand (Const.Int 1 (if val then 1 else 0)))
genExpr scope (Addition l r) = binOp scope l r Add
genExpr scope (Subtraction l r) = binOp scope l r Sub
genExpr scope (Multiplication l r) = binOp scope l r Mul
genExpr scope (Negation x) = binOp scope (Ast.ConstantInteger 0) x Sub
genExpr scope (ShiftLeft l r) = binOp scope l r Shl
genExpr scope (ShiftRight l r) = binOp scope l r (const LShr)
genExpr scope (Ast.And l r) = binOp scope l r (const (const AST.And))
genExpr scope (Ast.Or l r) = binOp scope l r (const (const AST.Or))
genExpr _ (Division _ _) = undefined
genExpr _ (Assignment _ _) = undefined
genExpr scope (CreateTuple ty [eleme]) = do
        gen <- genExpr scope eleme
        lbl <- genLabel
        tell [lbl := Trunc gen ty []]
        return (LocalReference lbl)
genExpr scope (CreateTuple ty elems) = do
        elements <- mapM (genExpr scope) elems
        tuplePtr <- genLabel
        tell [tuplePtr := Alloca ty Nothing 32 []]
        insertElement (LocalReference tuplePtr) 0 elements
  where insertElement tuplePtr _ [] = return tuplePtr
        insertElement tuplePtr elemIndex (x:xs) = do
                labelElement <- genLabel
                tell [labelElement := InsertValue tuplePtr x [elemIndex] []]
                insertElement (LocalReference labelElement) (elemIndex + 1) xs
genExpr scope (MethodCall fn args) = do
        argsOp <- mapM (genExpr scope) args
        fnOp <- genExpr scope fn
        labelInst <- genLabel
        let instruction = labelInst := Call False CC.C [] (Right fnOp) (map (\x -> (x,[])) argsOp) [] []
        tell [instruction]
        return (LocalReference labelInst)

tellBlock :: StateWriter (Named Instruction) Operand -> (Operand -> Named Terminator) -> StateWriter BasicBlock Name
tellBlock expr terminator = do
        emitState <- get
        ((resultOperand, lastLabel), instructions) <- return $ runWriter . flip runStateT emitState $ expr
        put lastLabel
        labelInst <- genLabel
        tell [BasicBlock labelInst instructions (terminator $! resultOperand)]
        return labelInst

-- statement to gen -> label of next block -> label of generated block
genStatement :: Scope -> Statement -> Name -> StateWriter BasicBlock Name
genStatement scope (IfStatement expr ifTrue ifFalse) nextInstruction = do
        trueBlock <- genBlock scope ifTrue nextInstruction
        falseBlock <- genBlock scope ifFalse nextInstruction
        tellBlock (genExpr scope expr) (\conditionLabel -> Do $ CondBr conditionLabel trueBlock falseBlock [])
genStatement scope (WhileStatement expr body) nextInstruction =
        mfix (\retval -> do
                bodyLabel <- genBlock scope body retval
                tellBlock (genExpr scope expr) (\conditionLabel -> Do $ CondBr conditionLabel bodyLabel nextInstruction [])
        )
genStatement scope (ExprStatement expr) nextInstruction = -- TODO: Chain together multiple ExprStatements into single BasicBlock
        tellBlock (genExpr scope expr) (\_ -> Do $ Br nextInstruction [])
genStatement scope (Return (Just expr)) _ =
        tellBlock (genExpr scope expr) (\retvalue -> Do $ Ret (Just retvalue) [])
genStatement _ (Return Nothing) _ = do
        retLabel <- genLabel
        tell [BasicBlock retLabel [] (Do $ Ret Nothing [])]
        return retLabel
genStatement _ (Ast.Function attributes retType fnName args (Just block)) nextInstruction = do
        genFunc attributes retType fnName args block
        return nextInstruction
genStatement _ (Ast.Function attributes retType fnName args Nothing) nextInstruction = do
        genExtern attributes retType fnName args
        return nextInstruction

genBlock :: Scope -> Block -> Name -> StateWriter BasicBlock Name
genBlock _ [] next = return next -- fallthrough and jump to next block
genBlock scope [st] next = genStatement scope st next
genBlock scope (st:sts) next = genBlock scope sts next >>= genStatement scope st

stdCall :: CC.CallingConvention
stdCall = CC.Numbered 64

genExtern :: [Attributes] -> Type -> String -> [(Type, String)] -> StateWriter BasicBlock ()
genExtern attributes retType fnName args =
        let method = functionDefaults {
         G.returnType = retType,
         G.name = Name fnName,
         G.parameters = (map (\(t, s) -> Parameter t (Name s) []) args, False) } in
         let m1 = if Ast.DllImport `elem` attributes then method{ G.linkage = Linkage.DLLImport } else method in
         let m2 = if Ast.StdCall `elem` attributes then m1{ G.callingConvention = stdCall } else m1 in
          modify (\st -> st{globals = m2 : globals st})

genFunc :: [Attributes] -> Type -> String -> [(Type, String)] -> Block -> StateWriter BasicBlock ()
genFunc attributes retType fnName args block = do
        st <- get
        let monadResult = do retLabel <- genStatement args (Return Nothing) undefined; genBlock args block retLabel
        let ((_, newSt), instructions) = runWriter . flip runStateT st $ monadResult
        let method = functionDefaults { 
         G.returnType = retType,
         G.name = Name fnName,
         G.parameters = (map (\(t, s) -> Parameter t (Name s) []) args, False),
         G.basicBlocks = reverse instructions }
        put newSt{ globals = method : globals newSt }
        return (if null attributes then () else error ("Attributes not empty for " ++ fnName))
        
--genFunc fnName args block = let monadResult = (do retLabel <- genStatement (Return Nothing) undefined; genBlock block retLabel) in
--        let (_, instructions) = runWriter . flip runStateT 1 $ monadResult in
--        reverse instructions -- Blocks come out in reverse order

emit :: Block -> [Global]
emit statement = let ((_, EmitState _ resultGlobals), _) = runWriter . flip runStateT (EmitState 1 []) $ genFunc [] (IntegerType 32) "__main" [] statement in resultGlobals
