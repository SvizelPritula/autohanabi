module Ansi where

makeRed :: String -> String
makeRed text = "\x1b[31m" ++ text ++ "\x1b[39m"

makeYellow :: String -> String
makeYellow text = "\x1b[33m" ++ text ++ "\x1b[39m"

makeGreen :: String -> String
makeGreen text = "\x1b[32m" ++ text ++ "\x1b[39m"

makeBlue :: String -> String
makeBlue text = "\x1b[34m" ++ text ++ "\x1b[39m"

makeWhite :: String -> String
makeWhite text = "\x1b[37m" ++ text ++ "\x1b[39m"

makeGray :: String -> String
makeGray text = "\x1b[2m" ++ text ++ "\x1b[22m"
