module LlvmCompile where

import LLVM.General.Target
import LLVM.General.AST as Ast
import LLVM.General.Module as Mod
import LLVM.General.Context
import Control.Monad.Error
import Control.Applicative

toModule :: [Global] -> Ast.Module
toModule b = Module "fzoo" Nothing Nothing (map GlobalDefinition b)

uneither :: Either a a -> a
uneither (Left x) = x
uneither (Right x) = x

extractError :: (Monad m) => ErrorT String m a -> m a
extractError = liftM switch . runErrorT
  where switch (Left x) = error x
        switch (Right x) = x

printMod :: Ast.Module -> IO String
printMod m = liftM derp $ opMod m moduleString
  where derp (Left x) = x ++ " Left"
        derp (Right x) = x ++ " Right"

opMod :: Ast.Module -> (Mod.Module -> IO a) -> IO (Either String a)
opMod m f = withContext (\context -> runErrorT $ withModuleFromAST context m f)

writeMod :: Ast.Module -> IO ()
writeMod m = withContext $ \context ->
             extractError $ withDefaultTargetMachine $ \machine ->
             extractError $ withModuleFromAST context m $ \llvmMod ->
             extractError $ writeObjectToFile machine "a.obj" llvmMod

compile :: [Global] -> IO String
compile g = let astMod = toModule g in printMod astMod <* writeMod astMod
