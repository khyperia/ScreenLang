module ScreenLangPrelude where

prelude :: String
-- fn i32 getchar(); fn i32 putchar(i32 arg); 
prelude = "fn i32 getchar();\n\
\fn i32 putchar(i32 arg);"
