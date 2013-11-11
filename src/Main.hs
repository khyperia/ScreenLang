module Main where

import Parser
import LlvmEmit
import LlvmCompile
import System.IO

process :: String -> IO String
process = derp . fmap (emit . addPrelude) . parse "<stdin>"
  where derp (Left l) = return (show l)
        derp (Right x) = compile x

main::IO()
main = do
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        content <- getLine
        result <- process content
        putStr result
